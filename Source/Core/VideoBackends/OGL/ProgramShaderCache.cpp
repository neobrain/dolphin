// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <string>

#include "Common/MathUtil.h"
#include "Common/StringUtil.h"

#include "VideoBackends/OGL/ProgramShaderCache.h"
#include "VideoBackends/OGL/Render.h"
#include "VideoBackends/OGL/StreamBuffer.h"

#include "VideoCommon/Debugger.h"
#include "VideoCommon/DriverDetails.h"
#include "VideoCommon/ImageWrite.h"
#include "VideoCommon/PixelShaderManager.h"
#include "VideoCommon/Statistics.h"
#include "VideoCommon/VertexShaderManager.h"

namespace OGL
{

static const u32 UBO_LENGTH = 32*1024*1024;

u32 ProgramShaderCache::s_ubo_buffer_size;
s32 ProgramShaderCache::s_ubo_align;

static StreamBuffer *s_buffer;
static int num_failures = 0;

static LinearDiskCache<SHADERUID, u8> g_program_disk_cache;
static GLuint CurrentProgram = 0;
ProgramShaderCache::PCache ProgramShaderCache::pshaders;
ProgramShaderCache::PCacheEntry* ProgramShaderCache::last_entry;
SHADERUID ProgramShaderCache::last_uid;
UidChecker<PixelShaderUid,PixelShaderCode> ProgramShaderCache::pixel_uid_checker;
UidChecker<VertexShaderUid,VertexShaderCode> ProgramShaderCache::vertex_uid_checker;

static std::string s_glsl_header = "";

static std::string GetGLSLVersionString()
{
	GLSL_VERSION v = g_ogl_config.eSupportedGLSLVersion;
	switch(v)
	{
		case GLSLES_300:
			return "#version 300 es";
		case GLSLES_310:
			return "#version 310 es";
		case GLSL_130:
			return "#version 130";
		case GLSL_140:
			return "#version 140";
		case GLSL_150:
			return "#version 150";
	}
	// Shouldn't ever hit this
	return "#version ERROR";
}

void SHADER::SetProgramVariables()
{
	// Bind UBO and texture samplers
	if (!g_ActiveConfig.backend_info.bSupportsBindingLayout)
	{
		// glsl shader must be bind to set samplers if we don't support binding layout
		Bind();

		GLint PSBlock_id = glGetUniformBlockIndex(glprogid, "PSBlock");
		GLint VSBlock_id = glGetUniformBlockIndex(glprogid, "VSBlock");

		if (PSBlock_id != -1)
			glUniformBlockBinding(glprogid, PSBlock_id, 1);
		if (VSBlock_id != -1)
			glUniformBlockBinding(glprogid, VSBlock_id, 2);

		// Bind Texture Sampler
		for (int a = 0; a <= 9; ++a)
		{
			char name[8];
			snprintf(name, 8, "samp%d", a);

			// Still need to get sampler locations since we aren't binding them statically in the shaders
			int loc = glGetUniformLocation(glprogid, name);
			if (loc != -1)
				glUniform1i(loc, a);
		}
	}
}

void SHADER::SetProgramBindings()
{
	if (g_ActiveConfig.backend_info.bSupportsDualSourceBlend)
	{
		// So we do support extended blending
		// So we need to set a few more things here.
		// Bind our out locations
		glBindFragDataLocationIndexed(glprogid, 0, 0, "ocol0");
		glBindFragDataLocationIndexed(glprogid, 0, 1, "ocol1");
	}
	// Need to set some attribute locations
	glBindAttribLocation(glprogid, SHADER_POSITION_ATTRIB, "rawpos");

	glBindAttribLocation(glprogid, SHADER_POSMTX_ATTRIB,   "posmtx");

	glBindAttribLocation(glprogid, SHADER_COLOR0_ATTRIB,   "color0");
	glBindAttribLocation(glprogid, SHADER_COLOR1_ATTRIB,   "color1");

	glBindAttribLocation(glprogid, SHADER_NORM0_ATTRIB,    "rawnorm0");
	glBindAttribLocation(glprogid, SHADER_NORM1_ATTRIB,    "rawnorm1");
	glBindAttribLocation(glprogid, SHADER_NORM2_ATTRIB,    "rawnorm2");

	for (int i=0; i<8; i++) {
		char attrib_name[8];
		snprintf(attrib_name, 8, "tex%d", i);
		glBindAttribLocation(glprogid, SHADER_TEXTURE0_ATTRIB+i, attrib_name);
	}
}

void SHADER::Bind()
{
	if (CurrentProgram != glprogid)
	{
		INCSTAT(stats.thisFrame.numShaderChanges);
		glUseProgram(glprogid);
		CurrentProgram = glprogid;
	}
}

void ProgramShaderCache::UploadConstants()
{
	if (PixelShaderManager::dirty || VertexShaderManager::dirty)
	{
		auto buffer = s_buffer->Map(s_ubo_buffer_size, s_ubo_align);

		memcpy(buffer.first,
			&PixelShaderManager::constants, sizeof(PixelShaderConstants));

		memcpy(buffer.first + ROUND_UP(sizeof(PixelShaderConstants), s_ubo_align),
			&VertexShaderManager::constants, sizeof(VertexShaderConstants));

		s_buffer->Unmap(s_ubo_buffer_size);
		glBindBufferRange(GL_UNIFORM_BUFFER, 1, s_buffer->m_buffer, buffer.second,
					sizeof(PixelShaderConstants));
		glBindBufferRange(GL_UNIFORM_BUFFER, 2, s_buffer->m_buffer, buffer.second + ROUND_UP(sizeof(PixelShaderConstants), s_ubo_align),
					sizeof(VertexShaderConstants));

		PixelShaderManager::dirty = false;
		VertexShaderManager::dirty = false;

		ADDSTAT(stats.thisFrame.bytesUniformStreamed, s_ubo_buffer_size);
	}
}

GLuint ProgramShaderCache::GetCurrentProgram(void)
{
	return CurrentProgram;
}

SHADER* ProgramShaderCache::SetShader ( DSTALPHA_MODE dstAlphaMode, u32 components )
{
	SHADERUID uid;
	GetShaderId(&uid, dstAlphaMode, components);

	// Check if the shader is already set
	if (last_entry)
	{
		if (uid == last_uid)
		{
			GFX_DEBUGGER_PAUSE_AT(NEXT_PIXEL_SHADER_CHANGE, true);
			last_entry->shader.Bind();
			return &last_entry->shader;
		}
	}

	last_uid = uid;

	// Check if shader is already in cache
	PCache::iterator iter = pshaders.find(uid);
	if (iter != pshaders.end())
	{
		PCacheEntry *entry = &iter->second;
		last_entry = entry;

		GFX_DEBUGGER_PAUSE_AT(NEXT_PIXEL_SHADER_CHANGE, true);
		last_entry->shader.Bind();
		return &last_entry->shader;
	}

	// Make an entry in the table
	PCacheEntry& newentry = pshaders[uid];
	last_entry = &newentry;
	newentry.in_cache = 0;

	VertexShaderCode vcode;
	PixelShaderCode pcode;
	GenerateVertexShaderCode(vcode, components, API_OPENGL);
	GeneratePixelShaderCode(pcode, dstAlphaMode, API_OPENGL, components);

	if (g_ActiveConfig.bEnableShaderDebugging)
	{
		newentry.shader.strvprog = vcode.GetBuffer();
		newentry.shader.strpprog = pcode.GetBuffer();
	}

#if defined(_DEBUG) || defined(DEBUGFAST)
	if (g_ActiveConfig.iLog & CONF_SAVESHADERS)
	{
		static int counter = 0;
		std::string filename =  StringFromFormat("%svs_%04i.txt", File::GetUserPath(D_DUMP_IDX).c_str(), counter++);
		SaveData(filename, vcode.GetBuffer());

		filename = StringFromFormat("%sps_%04i.txt", File::GetUserPath(D_DUMP_IDX).c_str(), counter++);
		SaveData(filename, pcode.GetBuffer());
	}
#endif

	if (!CompileShader(newentry.shader, vcode.GetBuffer(), pcode.GetBuffer(), s_glsl_header, s_glsl_header, g_ogl_config)) {
		GFX_DEBUGGER_PAUSE_AT(NEXT_ERROR, true);
		return nullptr;
	}

	INCSTAT(stats.numPixelShadersCreated);
	SETSTAT(stats.numPixelShadersAlive, pshaders.size());
	GFX_DEBUGGER_PAUSE_AT(NEXT_PIXEL_SHADER_CHANGE, true);

	last_entry->shader.Bind();
	return &last_entry->shader;
}

bool ProgramShaderCache::CompileShader(SHADER& shader, const char* vcode, const char* pcode, const std::string& vs_header, const std::string& ps_header, const VideoConfig& ogl_config)
{
	GLuint vsid = CompileSingleShader(GL_VERTEX_SHADER, vcode, vs_header, ogl_config);
	GLuint psid = CompileSingleShader(GL_FRAGMENT_SHADER, pcode, ps_header, ogl_config);

	if (!vsid || !psid)
	{
		glDeleteShader(vsid);
		glDeleteShader(psid);
		return false;
	}

	GLuint pid = shader.glprogid = glCreateProgram();

	glAttachShader(pid, vsid);
	glAttachShader(pid, psid);

	if (g_ogl_config.bSupportsGLSLCache)
		glProgramParameteri(pid, GL_PROGRAM_BINARY_RETRIEVABLE_HINT, GL_TRUE);

	shader.SetProgramBindings();

	glLinkProgram(pid);

	// original shaders aren't needed any more
	glDeleteShader(vsid);
	glDeleteShader(psid);

	GLint linkStatus;
	glGetProgramiv(pid, GL_LINK_STATUS, &linkStatus);
	GLsizei length = 0;
	glGetProgramiv(pid, GL_INFO_LOG_LENGTH, &length);
	if (linkStatus != GL_TRUE || (length > 1 && DEBUG_GLSL))
	{
		GLsizei charsWritten;
		GLchar* infoLog = new GLchar[length];
		glGetProgramInfoLog(pid, length, &charsWritten, infoLog);
		ERROR_LOG(VIDEO, "Program info log:\n%s", infoLog);

		std::string filename = StringFromFormat("%sbad_p_%d.txt", File::GetUserPath(D_DUMP_IDX).c_str(), num_failures++);
		std::ofstream file;
		OpenFStream(file, filename, std::ios_base::out);
		file << s_glsl_header << vcode << s_glsl_header << pcode << infoLog;
		file.close();

		if (linkStatus != GL_TRUE)
			PanicAlert("Failed to link shaders!\nThis usually happens when trying to use Dolphin with an outdated GPU or integrated GPU like the Intel GMA series.\n\nIf you're sure this is Dolphin's error anyway, post the contents of %s along with this error message at the forums.\n\nDebug info (%s, %s, %s):\n%s",
				filename.c_str(),
				g_ogl_config.gl_vendor,
				g_ogl_config.gl_renderer,
				g_ogl_config.gl_version,
				infoLog);

		delete [] infoLog;
	}
	if (linkStatus != GL_TRUE)
	{
		// Compile failed
		ERROR_LOG(VIDEO, "Program linking failed; see info log");

		// Don't try to use this shader
		glDeleteProgram(pid);
		return false;
	}

	shader.SetProgramVariables();

	return true;
}

GLuint ProgramShaderCache::CompileSingleShader(GLuint type, const char* code, const std::string& header, const VideoConfig& ogl_config)
{
	GLuint result = glCreateShader(type);

	const char *src[] = { header.c_str(), code };

	glShaderSource(result, 2, src, nullptr);
	glCompileShader(result);
	GLint compileStatus;
	glGetShaderiv(result, GL_COMPILE_STATUS, &compileStatus);
	GLsizei length = 0;
	glGetShaderiv(result, GL_INFO_LOG_LENGTH, &length);

	if (DriverDetails::HasBug(DriverDetails::BUG_BROKENINFOLOG))
		length = 1024;

	if (compileStatus != GL_TRUE || (length > 1 && DEBUG_GLSL))
	{
		GLsizei charsWritten;
		GLchar* infoLog = new GLchar[length];
		glGetShaderInfoLog(result, length, &charsWritten, infoLog);
		ERROR_LOG(VIDEO, "%s Shader info log:\n%s", type==GL_VERTEX_SHADER ? "VS" : "PS", infoLog);

		std::string filename = StringFromFormat("%sbad_%s_%04i.txt",
			File::GetUserPath(D_DUMP_IDX).c_str(),
			type==GL_VERTEX_SHADER ? "vs" : "ps",
			num_failures++);
		std::ofstream file;
		OpenFStream(file, filename, std::ios_base::out);
		file << header << code << infoLog;
		file.close();

		if (compileStatus != GL_TRUE)
			PanicAlert("Failed to compile %s shader!\nThis usually happens when trying to use Dolphin with an outdated GPU or integrated GPU like the Intel GMA series.\n\nIf you're sure this is Dolphin's error anyway, post the contents of %s along with this error message at the forums.\n\nDebug info (%s, %s, %s):\n%s",
				type==GL_VERTEX_SHADER ? "vertex" : "pixel",
				filename.c_str(),
				ogl_config.gl_vendor,
				ogl_config.gl_renderer,
				ogl_config.gl_version,
				infoLog);

		delete[] infoLog;
	}
	if (compileStatus != GL_TRUE)
	{
		// Compile failed
		ERROR_LOG(VIDEO, "Shader compilation failed; see info log");

		// Don't try to use this shader
		glDeleteShader(result);
		return 0;
	}
	(void)GL_REPORT_ERROR();
	return result;
}

void ProgramShaderCache::GetShaderId(SHADERUID* uid, DSTALPHA_MODE dstAlphaMode, u32 components)
{
	GetPixelShaderUid(uid->puid, dstAlphaMode, API_OPENGL, components);
	GetVertexShaderUid(uid->vuid, components, API_OPENGL);

	if (g_ActiveConfig.bEnableShaderDebugging)
	{
		PixelShaderCode pcode;
		GeneratePixelShaderCode(pcode, dstAlphaMode, API_OPENGL, components);
		pixel_uid_checker.AddToIndexAndCheck(pcode, uid->puid, "Pixel", "p");

		VertexShaderCode vcode;
		GenerateVertexShaderCode(vcode, components, API_OPENGL);
		vertex_uid_checker.AddToIndexAndCheck(vcode, uid->vuid, "Vertex", "v");
	}
}

ProgramShaderCache::PCacheEntry ProgramShaderCache::GetShaderProgram(void)
{
	return *last_entry;
}

void ProgramShaderCache::Init(void)
{
	// We have to get the UBO alignment here because
	// if we generate a buffer that isn't aligned
	// then the UBO will fail.
	glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &s_ubo_align);

	s_ubo_buffer_size = ROUND_UP(sizeof(PixelShaderConstants), s_ubo_align) + ROUND_UP(sizeof(VertexShaderConstants), s_ubo_align);

	// We multiply by *4*4 because we need to get down to basic machine units.
	// So multiply by four to get how many floats we have from vec4s
	// Then once more to get bytes
	s_buffer = StreamBuffer::Create(GL_UNIFORM_BUFFER, UBO_LENGTH);

	// Read our shader cache, only if supported
	if (g_ogl_config.bSupportsGLSLCache && !g_Config.bEnableShaderDebugging)
	{
		GLint Supported;
		glGetIntegerv(GL_NUM_PROGRAM_BINARY_FORMATS, &Supported);
		if (!Supported)
		{
			ERROR_LOG(VIDEO, "GL_ARB_get_program_binary is supported, but no binary format is known. So disable shader cache.");
			g_ogl_config.bSupportsGLSLCache = false;
		}
		else
		{
			if (!File::Exists(File::GetUserPath(D_SHADERCACHE_IDX)))
				File::CreateDir(File::GetUserPath(D_SHADERCACHE_IDX));

			std::string cache_filename = StringFromFormat("%sogl-%s-shaders.cache", File::GetUserPath(D_SHADERCACHE_IDX).c_str(),
				SConfig::GetInstance().m_LocalCoreStartupParameter.m_strUniqueID.c_str());

			ProgramShaderCacheInserter inserter;
			g_program_disk_cache.OpenAndRead(cache_filename, inserter);
		}
		SETSTAT(stats.numPixelShadersAlive, pshaders.size());
	}

	s_glsl_header = CreateHeader(g_ActiveConfig, g_ogl_config);

	CurrentProgram = 0;
	last_entry = nullptr;
}

void ProgramShaderCache::Shutdown(void)
{
	// store all shaders in cache on disk
	if (g_ogl_config.bSupportsGLSLCache && !g_Config.bEnableShaderDebugging)
	{
		for (auto& entry : pshaders)
		{
			if (entry.second.in_cache)
			{
				continue;
			}

			GLint binary_size;
			glGetProgramiv(entry.second.shader.glprogid, GL_PROGRAM_BINARY_LENGTH, &binary_size);
			if (!binary_size)
			{
				continue;
			}

			u8 *data = new u8[binary_size+sizeof(GLenum)];
			u8 *binary = data + sizeof(GLenum);
			GLenum *prog_format = (GLenum*)data;
			glGetProgramBinary(entry.second.shader.glprogid, binary_size, nullptr, prog_format, binary);

			g_program_disk_cache.Append(entry.first, data, binary_size+sizeof(GLenum));
			delete [] data;
		}

		g_program_disk_cache.Sync();
		g_program_disk_cache.Close();
	}

	glUseProgram(0);

	for (auto& entry : pshaders)
	{
		entry.second.Destroy();
	}
	pshaders.clear();

	pixel_uid_checker.Invalidate();
	vertex_uid_checker.Invalidate();

	delete s_buffer;
	s_buffer = nullptr;
}

std::string ProgramShaderCache::CreateHeader(const ::VideoConfig& global_config, const OGL::VideoConfig& ogl_config)
{
	GLSL_VERSION v = g_ogl_config.eSupportedGLSLVersion;
	auto& backend_info = global_config.backend_info;

	std::string header;
	header += StringFromFormat("%s\n", GetGLSLVersionString().c_str());
	if (v < GLSL_140)
		header += "#extension GL_ARB_uniform_buffer_object : enable\n";

	if (backend_info.bSupportsEarlyZ)
		header += "#extension GL_ARB_shader_image_load_store : enable\n";

	if (backend_info.bSupportsBindingLayout && v < GLSLES_310)
		header += "#extension GL_ARB_shading_language_420pack : enable\n";

	if (ogl_config.bSupportsMSAA && v < GLSL_150)
		header += "#extension GL_ARB_texture_multisample : enable\n";

	if (ogl_config.bSupportSampleShading)
		header += "#extension GL_ARB_sample_shading : enable\n";

	if (backend_info.bSupportsBindingLayout)
		header += "#define SAMPLER_BINDING(x) layout(binding = x)\n";
	else
		header += "#define SAMPLER_BINDING(x)\n";

	if (v >= GLSLES_300)
	{
		header += "precision highp float;\n";
		header += "precision highp int;\n";
	}

	// HLSL to GLSL translation
	header += "#define float2 vec2\n";
	header += "#define float3 vec3\n";
	header += "#define float4 vec4\n";
	header += "#define uint2 uvec2\n";
	header += "#define uint3 uvec3\n";
	header += "#define uint4 uvec4\n";
	header += "#define int2 ivec2\n";
	header += "#define int3 ivec3\n";
	header += "#define int4 ivec4\n";
	header += "#define frac fract\n";
	header += "#define lerp mix\n";

	if (DriverDetails::HasBug(DriverDetails::BUG_BROKENTEXTURESIZE))
		header += "#define textureSize(x, y) ivec2(1, 1)\n";

	if (DriverDetails::HasBug(DriverDetails::BUG_BROKENCENTROID))
		header += "#define centroid\n";

	return header;
}


void ProgramShaderCache::ProgramShaderCacheInserter::Read ( const SHADERUID& key, const u8* value, u32 value_size )
{
	const u8 *binary = value+sizeof(GLenum);
	GLenum *prog_format = (GLenum*)value;
	GLint binary_size = value_size-sizeof(GLenum);

	PCacheEntry entry;
	entry.in_cache = 1;
	entry.shader.glprogid = glCreateProgram();
	glProgramBinary(entry.shader.glprogid, *prog_format, binary, binary_size);

	GLint success;
	glGetProgramiv(entry.shader.glprogid, GL_LINK_STATUS, &success);

	if (success)
	{
		pshaders[key] = entry;
		entry.shader.SetProgramVariables();
	}
	else
		glDeleteProgram(entry.shader.glprogid);
}


} // namespace OGL
