// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Common/Common.h"
#include "Common/MemoryUtil.h"

#include "VideoBackends/Software/BPMemLoader.h"
#include "VideoBackends/Software/DebugUtil.h"
#include "VideoBackends/Software/HwRasterizer.h"
#include "VideoBackends/Software/NativeVertexFormat.h"

#include "VideoCommon/VideoCommon.h"
#include <VideoCommon/TextureCacheBase.h>
#include <VideoCommon/PixelShaderManager.h>

#include "SWRenderer.h"
#include <VideoBackends/OGL/StreamBuffer.h>
#include <VideoBackends/OGL/VertexManager.h>
#include <VideoBackends/OGL/TextureCache.h>

#include "Tev.h"
#define TEMP_SIZE (1024*1024*4)

namespace Rasterizer {
extern Tev tev;
}

namespace HwRasterizer
{
	static float efbHalfWidth;
	static float efbHalfHeight;
	static bool hasTexture;

	static u8 *temp;

	// Programs
	static GLuint colProg, texProg, clearProg;

	// Texture type
	static GLenum texType;

	// Color
	static GLint col_apos = -1, col_atex = -1;
	// Tex
	static GLint tex_apos = -1, tex_atex = -1, tex_utex = -1;
	// Clear shader
	static GLint clear_apos = -1, clear_ucol = -1;

	static OGL::StreamBuffer *s_vertexBuffer;
	static OGL::StreamBuffer *s_indexBuffer;
	static OGL::StreamBuffer *s_uniformBuffer;

	static void CreateShaders()
	{
		// Color Vertices
		static const char *fragcolText130 =
			"#version 130\n"
			"#ifdef GL_ES\n"
			"precision highp float;\n"
			"#endif\n"
			"varying vec4 TexCoordOut;\n"
			"void main() {\n"
			"	gl_FragColor = TexCoordOut;\n"
			"}\n";
		// Texture Vertices
		static const char *fragtexText130 =
			"#version 130\n"
			"varying vec4 TexCoordOut;\n"
			"uniform sampler2D Texture;\n"
			"void main() {\n"
			"	gl_FragColor = texture(Texture, TexCoordOut.xy);\n"
			"}\n";
		// Clear shader
		static const char *fragclearText =
			"#ifdef GL_ES\n"
			"precision highp float;\n"
			"#endif\n"
			"uniform vec4 Color;\n"
			"void main() {\n"
			"	gl_FragColor = Color;\n"
			"}\n";
		// Generic passthrough vertex shaders
		static const char *vertShaderText130 =
			"#version 130\n"
			"attribute vec4 pos;\n"
			"attribute vec4 TexCoordIn;\n "
			"varying vec4 TexCoordOut;\n "
			"void main() {\n"
			"	gl_Position = pos;\n"
			"	TexCoordOut = TexCoordIn;\n"
			"}\n";
		static const char *vertclearText =
			"attribute vec4 pos;\n"
			"void main() {\n"
			"	gl_Position = pos;\n"
			"}\n";

		// Color Program
		colProg = OpenGL_CompileProgram(vertShaderText130, fragcolText130);

		// Texture Program
		texProg = OpenGL_CompileProgram(vertShaderText130, fragtexText130);

		// Clear Program
		clearProg = OpenGL_CompileProgram(vertclearText, fragclearText);

		// Color attributes
		col_apos = glGetAttribLocation(colProg, "pos");
		col_atex = glGetAttribLocation(colProg, "TexCoordIn");
		// Texture attributes
		tex_apos = glGetAttribLocation(texProg, "pos");
		tex_atex = glGetAttribLocation(texProg, "TexCoordIn");
		tex_utex = glGetUniformLocation(texProg, "Texture");
		// Clear attributes
		clear_apos = glGetAttribLocation(clearProg, "pos");
		clear_ucol = glGetUniformLocation(clearProg, "Color");

		s_vertexBuffer = OGL::StreamBuffer::Create(GL_ARRAY_BUFFER, 512);
		s_indexBuffer = OGL::StreamBuffer::Create(GL_ELEMENT_ARRAY_BUFFER, 512);
		s_uniformBuffer = OGL::StreamBuffer::Create(GL_UNIFORM_BUFFER, 65536);
		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
		glBindBuffer(GL_UNIFORM_BUFFER, 0);
		g_texture_cache = new OGL::TextureCache;
		PixelShaderManager::Init();
	}

	void Init()
	{
		efbHalfWidth = EFB_WIDTH / 2.0f;
//		efbHalfHeight = 480 / 2.0f; // TODO: WTF? should be 528 instead, shouldn't it?
		efbHalfHeight = EFB_HEIGHT / 2.0f;

		temp = (u8*)AllocateMemoryPages(TEMP_SIZE);
	}
	void Shutdown()
	{
		glDeleteProgram(colProg);
		glDeleteProgram(texProg);
		glDeleteProgram(clearProg);
		glBindBuffer(GL_ARRAY_BUFFER, 0 );
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0 );

		delete s_vertexBuffer;
		delete s_indexBuffer;
		delete g_texture_cache;
		g_texture_cache = nullptr;
	}
	void Prepare()
	{
		//legacy multitexturing: select texture channel only.
		glActiveTexture(GL_TEXTURE0);
		glPixelStorei(GL_UNPACK_ALIGNMENT, 4);  // 4-byte pixel alignment
		if (GLInterface->GetMode() == GLInterfaceMode::MODE_OPENGL)
		{
			glShadeModel(GL_SMOOTH);
			glDisable(GL_BLEND);
			glClearDepth(1.0f);
			glEnable(GL_SCISSOR_TEST);
			glDisable(GL_LIGHTING);
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();

			glClientActiveTexture(GL_TEXTURE0);
			glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glEnable(GL_TEXTURE_RECTANGLE_ARB);
			glStencilFunc(GL_ALWAYS, 0, 0);
			glDisable(GL_STENCIL_TEST);
		}
		// used by hw rasterizer if it enables blending and depth test
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glDepthFunc(GL_LEQUAL);

		glClearColor(0.0f, 0.0f, 0.0f, 0.0f);

		CreateShaders();
		if (GLInterface->GetMode() == GLInterfaceMode::MODE_OPENGL)
			texType = GL_TEXTURE_RECTANGLE;
		else
			texType = GL_TEXTURE_2D;

		GL_REPORT_ERRORD();
	}

	void BeginTriangles()
	{
		// disabling depth test sometimes allows more things to be visible
		glEnable(GL_DEPTH_TEST);
		glEnable(GL_BLEND);

//		bpmem.genMode.numtexgens = std::min(bpmem.genMode.numtexgens, xfregs.numTexGen.numTexGens);
//		hasTexture = bpmem.tevorders[0].enable0 && (xfregs.numTexGen.numTexGens>0) && (bpmem.genMode.numtexgens>0);
		hasTexture = xfregs.numTexGen.numTexGens > 0;
	}

	void EndTriangles()
	{
		glBindTexture(texType, 0);
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_BLEND);
	}

	void SetupState()
	{
		// SetGenerationMode
		{
			// Culling should've already been done by the Clipper!
			glDisable(GL_CULL_FACE);
		}

		// SetScissor
		{
			const int xoff = bpmem.scissorOffset.x * 2 - 342;
			const int yoff = bpmem.scissorOffset.y * 2 - 342;

			EFBRectangle rc (bpmem.scissorTL.x - xoff - 342, bpmem.scissorTL.y - yoff - 342,
							bpmem.scissorBR.x - xoff - 341, bpmem.scissorBR.y - yoff - 341);

			if (rc.left < 0) rc.left = 0;
			if (rc.top < 0) rc.top = 0;
			if (rc.right > EFB_WIDTH) rc.right = EFB_WIDTH;
			if (rc.bottom > EFB_HEIGHT) rc.bottom = EFB_HEIGHT;

			if (rc.left > rc.right) rc.right = rc.left;
			if (rc.top > rc.bottom) rc.bottom = rc.top;

//			glEnable(GL_SCISSOR_TEST);
//			glScissor(rc.left, EFB_HEIGHT - rc.bottom, rc.GetWidth(), rc.GetHeight());
		}

		// SetColorMask
		{
			// Only enable alpha channel if it's supported by the current EFB format
			GLenum ColorMask = GL_FALSE, AlphaMask = GL_FALSE;
			if (bpmem.alpha_test.TestResult() != AlphaTest::FAIL)
			{
				if (bpmem.blendmode.colorupdate)
					ColorMask = GL_TRUE;
				if (bpmem.blendmode.alphaupdate && (bpmem.zcontrol.pixel_format == PEControl::RGBA6_Z24))
					AlphaMask = GL_TRUE;
			}
			glColorMask(ColorMask,  ColorMask,  ColorMask,  AlphaMask);
		}

		// SetDepthMode
		{
			const GLenum glCmpFuncs[8] =
			{
				GL_NEVER,
				GL_LESS,
				GL_EQUAL,
				GL_LEQUAL,
				GL_GREATER,
				GL_NOTEQUAL,
				GL_GEQUAL,
				GL_ALWAYS
			};

			if (bpmem.zmode.testenable)
			{
				glEnable(GL_DEPTH_TEST);
				glDepthMask(bpmem.zmode.updateenable ? GL_TRUE : GL_FALSE);
				glDepthFunc(glCmpFuncs[bpmem.zmode.func]);
			}
			else
			{
				// if the test is disabled write is disabled too
				// TODO: When PE performance metrics are being emulated via occlusion queries, we should (probably?) enable depth test with depth function ALWAYS here
				glDisable(GL_DEPTH_TEST);
				glDepthMask(GL_FALSE);
			}
		}

		// SetBlendMode(true);
		{
			// Our render target always uses an alpha channel, so we need to override the blend functions to assume a destination alpha of 1 if the render target isn't supposed to have an alpha channel
			// Example: D3DBLEND_DESTALPHA needs to be D3DBLEND_ONE since the result without an alpha channel is assumed to always be 1.
			bool target_has_alpha = bpmem.zcontrol.pixel_format == PEControl::RGBA6_Z24;
			bool useDstAlpha = bpmem.dstalpha.enable && bpmem.blendmode.alphaupdate && target_has_alpha;
			bool useDualSource = useDstAlpha;

			const GLenum glSrcFactors[8] =
			{
				GL_ZERO,
				GL_ONE,
				GL_DST_COLOR,
				GL_ONE_MINUS_DST_COLOR,
				(useDualSource)  ? GL_SRC1_ALPHA : (GLenum)GL_SRC_ALPHA,
				(useDualSource)  ? GL_ONE_MINUS_SRC1_ALPHA : (GLenum)GL_ONE_MINUS_SRC_ALPHA,
				(target_has_alpha) ? GL_DST_ALPHA : (GLenum)GL_ONE,
				(target_has_alpha) ? GL_ONE_MINUS_DST_ALPHA : (GLenum)GL_ZERO
			};
			const GLenum glDestFactors[8] =
			{
				GL_ZERO,
				GL_ONE,
				GL_SRC_COLOR,
				GL_ONE_MINUS_SRC_COLOR,
				(useDualSource)  ? GL_SRC1_ALPHA : (GLenum)GL_SRC_ALPHA,
				(useDualSource)  ? GL_ONE_MINUS_SRC1_ALPHA : (GLenum)GL_ONE_MINUS_SRC_ALPHA,
				(target_has_alpha) ? GL_DST_ALPHA : (GLenum)GL_ONE,
				(target_has_alpha) ? GL_ONE_MINUS_DST_ALPHA : (GLenum)GL_ZERO
			};

			// blend mode bit mask
			// 0 - blend enable
			// 1 - dst alpha enabled
			// 2 - reverse subtract enable (else add)
			// 3-5 - srcRGB function
			// 6-8 - dstRGB function

			u32 newval = useDualSource << 1;
			newval |= bpmem.blendmode.subtract << 2;

			if (bpmem.blendmode.subtract)
				newval |= 0x0049;   // enable blending src 1 dst 1
			else if (bpmem.blendmode.blendenable)
			{
				newval |= 1;    // enable blending
				newval |= bpmem.blendmode.srcfactor << 3;
				newval |= bpmem.blendmode.dstfactor << 6;
			}

			if (newval & 1)
				glEnable(GL_BLEND);
			else
				glDisable(GL_BLEND);

			{
				// subtract enable change
				GLenum equation = newval & 4 ? GL_FUNC_REVERSE_SUBTRACT : GL_FUNC_ADD;
				GLenum equationAlpha = useDualSource ? GL_FUNC_ADD : equation;

				glBlendEquationSeparate(equation, equationAlpha);
			}

			{
				u32 srcidx = (newval >> 3) & 7;
				u32 dstidx = (newval >> 6) & 7;
				GLenum srcFactor = glSrcFactors[srcidx];
				GLenum dstFactor = glDestFactors[dstidx];

				// adjust alpha factors
				if (useDualSource)
				{
					srcidx = BlendMode::ONE;
					dstidx = BlendMode::ZERO;
				}
				else
				{
					// we can't use GL_DST_COLOR or GL_ONE_MINUS_DST_COLOR for source in alpha channel so use their alpha equivalent instead
					if (srcidx == BlendMode::DSTCLR) srcidx = BlendMode::DSTALPHA;
					if (srcidx == BlendMode::INVDSTCLR) srcidx = BlendMode::INVDSTALPHA;

					// we can't use GL_SRC_COLOR or GL_ONE_MINUS_SRC_COLOR for destination in alpha channel so use their alpha equivalent instead
					if (dstidx == BlendMode::SRCCLR) dstidx = BlendMode::SRCALPHA;
					if (dstidx == BlendMode::INVSRCCLR) dstidx = BlendMode::INVSRCALPHA;
				}
				GLenum srcFactorAlpha = glSrcFactors[srcidx];
				GLenum dstFactorAlpha = glDestFactors[dstidx];
				// blend RGB change
				glBlendFuncSeparate(srcFactor, dstFactor, srcFactorAlpha, dstFactorAlpha);
			}
		}

		// SetLogicOpMode
		{
			// Logic ops aren't available in GLES3/GLES2
			const GLenum glLogicOpCodes[16] =
			{
				GL_CLEAR,
				GL_AND,
				GL_AND_REVERSE,
				GL_COPY,
				GL_AND_INVERTED,
				GL_NOOP,
				GL_XOR,
				GL_OR,
				GL_NOR,
				GL_EQUIV,
				GL_INVERT,
				GL_OR_REVERSE,
				GL_COPY_INVERTED,
				GL_OR_INVERTED,
				GL_NAND,
				GL_SET
			};

			if (bpmem.blendmode.logicopenable)
			{
				glEnable(GL_COLOR_LOGIC_OP);
				glLogicOp(glLogicOpCodes[bpmem.blendmode.logicmode]);
			}
			else
			{
				glDisable(GL_COLOR_LOGIC_OP);
			}
		}
	}

	void DrawTriangleFrontFace(OutputVertexData *v0, OutputVertexData *v1, OutputVertexData *v2)
	{
		SetupState();

		// x+,y+ => top right
		// x-,y+ => top left
		// x+,y- => bottom right
		// x-,y- => bottom left
		float pos[9] = {
			(v0->screenPosition.x / efbHalfWidth) - 1.0f,
			1.0f - (v0->screenPosition.y / efbHalfHeight),
			v0->screenPosition.z / (float)0x00ffffff,

			(v1->screenPosition.x / efbHalfWidth) - 1.0f,
			1.0f - (v1->screenPosition.y / efbHalfHeight),
			v1->screenPosition.z / (float)0x00ffffff,

			(v2->screenPosition.x / efbHalfWidth) - 1.0f,
			1.0f - (v2->screenPosition.y / efbHalfHeight),
			v2->screenPosition.z / (float)0x00ffffff,
		};

		float r0 = v0->color[0][OutputVertexData::RED_C] / 255.0f;
		float g0 = v0->color[0][OutputVertexData::GRN_C] / 255.0f;
		float b0 = v0->color[0][OutputVertexData::BLU_C] / 255.0f;

		float r1 = v1->color[0][OutputVertexData::RED_C] / 255.0f;
		float g1 = v1->color[0][OutputVertexData::GRN_C] / 255.0f;
		float b1 = v1->color[0][OutputVertexData::BLU_C] / 255.0f;

		float r2 = v2->color[0][OutputVertexData::RED_C] / 255.0f;
		float g2 = v2->color[0][OutputVertexData::GRN_C] / 255.0f;
		float b2 = v2->color[0][OutputVertexData::BLU_C] / 255.0f;

		const GLfloat verts[3*3] = {
			pos[0], pos[1], pos[2],
			pos[3], pos[4], pos[5],
			pos[6], pos[7], pos[8]
		};
		const GLfloat col[3*4] = {
			 r0, g0, b0, 1.0f ,
			 r1, g1, b1, 1.0f ,
			 r2, g2, b2, 1.0f 
		};

		GLfloat tex[3*2*8];
		for (int i = 0; i < xfregs.numTexGen.numTexGens; ++i)
		{
			// Undoing texcoord scaling here for compatibility with PixelShaderGen
			tex[0+i*6] = v0->texCoords[i].x / (float)(bpmem.texcoords[i].s.scale_minus_1 + 1);
			tex[1+i*6] = v0->texCoords[i].y / (float)(bpmem.texcoords[i].t.scale_minus_1 + 1);
			tex[2+i*6] = v1->texCoords[i].x / (float)(bpmem.texcoords[i].s.scale_minus_1 + 1);
			tex[3+i*6] = v1->texCoords[i].y / (float)(bpmem.texcoords[i].t.scale_minus_1 + 1);
			tex[4+i*6] = v2->texCoords[i].x / (float)(bpmem.texcoords[i].s.scale_minus_1 + 1);
			tex[5+i*6] = v2->texCoords[i].y / (float)(bpmem.texcoords[i].t.scale_minus_1 + 1);
		}

		const u32 has_texcoord[8] = {
			g_VtxDesc.Tex0Coord, g_VtxDesc.Tex1Coord, g_VtxDesc.Tex2Coord, g_VtxDesc.Tex3Coord,
			g_VtxDesc.Tex4Coord, g_VtxDesc.Tex5Coord, g_VtxDesc.Tex6Coord, (const u32)((g_VtxDesc.Hex >> 31) & 3)
		};

		{
			if (hasTexture)
			{
				u32 usedtextures = 0;
				for (u32 i = 0; i < bpmem.genMode.numtevstages + 1u; ++i)
					if (bpmem.tevorders[i / 2].getEnable(i & 1))
						usedtextures |= 1 << bpmem.tevorders[i/2].getTexMap(i & 1);

				// TODO: Use imask for this instead
				if (bpmem.genMode.numindstages > 0)
					for (unsigned int i = 0; i < bpmem.genMode.numtevstages + 1u; ++i)
						if (bpmem.tevind[i].IsActive() && bpmem.tevind[i].bt < bpmem.genMode.numindstages)
							usedtextures |= 1 << bpmem.tevindref.getTexMap(bpmem.tevind[i].bt);

				for (unsigned int i = 0; i < 8; i++)
				{
		//			g_renderer->SetSamplerState(i & 3, i >> 2);
					const FourTexUnits &texparams = bpmem.tex[i >> 2];
					const OGL::TextureCache::TCacheEntryBase* tentry = g_texture_cache->Load(i,
						(texparams.texImage3[i&3].image_base/* & 0x1FFFFF*/) << 5,
						texparams.texImage0[i&3].width + 1, texparams.texImage0[i&3].height + 1,
						texparams.texImage0[i&3].format, texparams.texTlut[i&3].tmem_offset<<9,
						texparams.texTlut[i&3].tlut_format,
						((texparams.texMode0[i&3].min_filter & 3) != 0),
						(texparams.texMode1[i&3].max_lod + 0xf) / 0x10,
						(texparams.texImage1[i&3].image_type != 0));
				}
			}
			int vertex_stride = sizeof(float)*3;
			/*if (hasColor) */ vertex_stride += sizeof(float) * 4;
			for (int i = 0; i < 8; ++i)
				if (has_texcoord[i])
					vertex_stride += sizeof(float) * 2;

			std::pair<u8*,size_t> mapping = s_vertexBuffer->Map(512, vertex_stride);
			for (int i = 0; i < 3; ++i)
			{
				int offset = 0;
				memcpy(&mapping.first[vertex_stride*i], &verts[3*i], 3*sizeof(float));
				offset += 3 * sizeof(float);

				/*if (hasColor) */
				{
					memcpy(&mapping.first[vertex_stride*i + offset], &col[4*i], 4*sizeof(float));
					offset += 4 * sizeof(float);
				}

				for (int tc = 0; tc < 8; ++tc)
				{
					if (has_texcoord[tc])
					{
						memcpy(&mapping.first[vertex_stride*i + offset], &tex[2*i + 6*tc], 2*sizeof(float));
						offset += 2 * sizeof(float);
					}
				}
			}
			s_vertexBuffer->Unmap(vertex_stride*3);
			GL_REPORT_ERRORD();

			mapping = s_indexBuffer->Map(512 * sizeof(u16));
			((u16*)mapping.first)[0] = 0;
			((u16*)mapping.first)[1] = 1;
			((u16*)mapping.first)[2] = 2;
			s_indexBuffer->Unmap(3 * sizeof(u16));
			GL_REPORT_ERRORD();

			GLuint VAO;
			glGenVertexArrays(1, &VAO);
			glBindVertexArray(VAO);
			GL_REPORT_ERRORD();

			glBindBuffer(GL_ARRAY_BUFFER, s_vertexBuffer->m_buffer);
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, s_indexBuffer->m_buffer);

			static char vbuf[1024];
			static char pbuf[1024];

			ShaderCode vcode;
			vcode.SetBuffer(vbuf);
			vcode.Write("#version 130\n");
			vcode.Write("#extension GL_ARB_uniform_buffer_object : enable\n");

			vcode.Write("in vec4 rawpos; // ATTR%d,\n", SHADER_POSITION_ATTRIB);
//			if (hasColor)
				vcode.Write("in vec4 color0; // ATTR%d,\n", SHADER_COLOR0_ATTRIB);
			for (int tc = 0; tc < 8; ++tc)
				if (has_texcoord[tc])
					vcode.Write("in vec2 tex%d; // ATTR%d,\n", tc, SHADER_TEXTURE0_ATTRIB+tc);

			for (int tc = 0; tc < 8; ++tc)
				if (has_texcoord[tc])
					vcode.Write("centroid out vec3 uv%d_2;\n", tc);
			vcode.Write("centroid out vec4 clipPos_2;\n");
//			if (hasColor)
			vcode.Write("centroid out vec4 colors_02;\n");

			vcode.Write("void main()\n{\n");
			for (int tc = 0; tc < 8; ++tc)
				if (has_texcoord[tc])
					vcode.Write("uv%d_2 = vec3(tex%d, 0.0);\n", tc, tc);
			vcode.Write("clipPos_2 = rawpos;\n");
//			if (hasColor)
				vcode.Write("colors_02 = color0;\n");
			vcode.Write("gl_Position = rawpos;\n");

			vcode.Write("}\n");


			ShaderCode pcode;
			pcode.SetBuffer(pbuf);

			g_ActiveConfig.backend_info.bSupportsBindingLayout = true;
//			g_ActiveConfig.backend_info.bSupportsDualSourceBlend = true;// needs glBindFragDataLocationIndexed
//			g_ActiveConfig.backend_info.bSupportsEarlyZ = true;
//			g_ActiveConfig.backend_info.bSupportsOversizedViewports = true;

			u32 components = 0;
			components |= /*hasColor ? */ VB_HAS_COL0 /* : 0*/;
			for (int tc = 0; tc < 8; ++tc)
				if (has_texcoord[tc])
					components |= VB_HAS_UV0 + tc;

			PixelShaderUid ps_uid;
			GetPixelShaderUid(ps_uid, DSTALPHA_NONE, API_OPENGL, components);

			static std::map<PixelShaderUid, GLuint> programs;

			if (programs.find(ps_uid) == programs.end())
			{
				GLuint& programID = programs[ps_uid];
				ERROR_LOG(VIDEO, "Number of active programs: %d", programs.size());
				GeneratePixelShaderCode(pcode, DSTALPHA_NONE, API_OPENGL, components);

				// generate objects
				GLuint vertexShaderID = glCreateShader(GL_VERTEX_SHADER);
				GLuint fragmentShaderID = glCreateShader(GL_FRAGMENT_SHADER);
				programID = glCreateProgram();

				// compile vertex shader
				const char *src[] = { vcode.GetBuffer() };
				glShaderSource(vertexShaderID, 1, src, nullptr);
				glCompileShader(vertexShaderID);

				GLint compileStatus;
				glGetShaderiv(vertexShaderID, GL_COMPILE_STATUS, &compileStatus);
				GLsizei length2 = 0;
				glGetShaderiv(vertexShaderID, GL_INFO_LOG_LENGTH, &length2);

				if (compileStatus != GL_TRUE)
				{
					GLsizei charsWritten;
					GLchar* infoLog = new GLchar[length2];
					glGetShaderInfoLog(vertexShaderID, length2, &charsWritten, infoLog);
					ERROR_LOG(VIDEO, "VS Shader info log:\n%s", infoLog);
				}

				// compile fragment shader
				static char s_glsl_header[512];
				snprintf(s_glsl_header, sizeof(s_glsl_header),
					"#version 130\n"
					"#extension GL_ARB_uniform_buffer_object : enable\n" // ubo
					"#extension GL_ARB_shading_language_420pack : enable\n"
//					"%s\n" // early-z
//					"%s\n" // 420pack

					// Silly differences
					"#define float2 vec2\n"
					"#define float3 vec3\n"
					"#define float4 vec4\n"
					"#define uint2 uvec2\n"
					"#define uint3 uvec3\n"
					"#define uint4 uvec4\n"
					"#define int2 ivec2\n"
					"#define int3 ivec3\n"
					"#define int4 ivec4\n"

					// hlsl to glsl function translation
					"#define frac fract\n"
					"#define lerp mix\n"

//					, (g_ActiveConfig.backend_info.bSupportsBindingLayout && v < GLSLES_310) ? "#extension GL_ARB_shading_language_420pack : enable" : ""
				);

				const char *fsrc[] = { s_glsl_header, pcode.GetBuffer() };
				glShaderSource(fragmentShaderID, 2, fsrc, nullptr);
				glCompileShader(fragmentShaderID);

				glGetShaderiv(fragmentShaderID, GL_COMPILE_STATUS, &compileStatus);
				length2 = 0;
				glGetShaderiv(fragmentShaderID, GL_INFO_LOG_LENGTH, &length2);

				if (compileStatus != GL_TRUE)
				{
					GLsizei charsWritten;
					GLchar* infoLog = new GLchar[length2];
					glGetShaderInfoLog(fragmentShaderID, length2, &charsWritten, infoLog);
					ERROR_LOG(VIDEO, "PS Shader info log:\n%s", infoLog);
					ERROR_LOG(VIDEO, "%s\n", pcode.GetBuffer());
					delete[] infoLog;
				}

				// link them
				glAttachShader(programID, vertexShaderID);
				glAttachShader(programID, fragmentShaderID);

				glBindAttribLocation(programID, SHADER_POSITION_ATTRIB, "rawpos");
				glBindAttribLocation(programID, SHADER_COLOR0_ATTRIB,   "color0");
				glBindAttribLocation(programID, SHADER_TEXTURE0_ATTRIB,   "tex0");
				glBindAttribLocation(programID, SHADER_TEXTURE1_ATTRIB,   "tex1");
				glBindAttribLocation(programID, SHADER_TEXTURE2_ATTRIB,   "tex2");
				glBindAttribLocation(programID, SHADER_TEXTURE3_ATTRIB,   "tex3");
				glBindAttribLocation(programID, SHADER_TEXTURE4_ATTRIB,   "tex4");
				glBindAttribLocation(programID, SHADER_TEXTURE5_ATTRIB,   "tex5");
				glBindAttribLocation(programID, SHADER_TEXTURE6_ATTRIB,   "tex6");
				glBindAttribLocation(programID, SHADER_TEXTURE7_ATTRIB,   "tex7");

				glLinkProgram(programID);
				GLint linkStatus;
				glGetProgramiv(programID, GL_LINK_STATUS, &linkStatus);
				GLsizei length = 0;
				glGetProgramiv(programID, GL_INFO_LOG_LENGTH, &length);
				if (linkStatus != GL_TRUE || (length > 1 && DEBUG_GLSL))
				{
					GLsizei charsWritten;
					GLchar* infoLog = new GLchar[length];
					glGetProgramInfoLog(programID, length, &charsWritten, infoLog);
					ERROR_LOG(VIDEO, "yo.. linking failed! %s", infoLog);
				}

				// cleanup
				glDeleteShader(vertexShaderID);
				glDeleteShader(fragmentShaderID);
			}

			GLuint& programID = programs[ps_uid];

			// Set texture width and height
			// TODO: Set _all_ uniforms!
			{
				// TODO: Uploading constants does not seem to work, yet!
				glUseProgram(programID);

				PixelShaderManager::Dirty();
				for (int i = 0; i < 8; ++i)
				{
					PixelShaderManager::SetTexDims(i, bpmem.tex[i/2].texImage0[i%4].width, bpmem.tex[i/2].texImage0[i%4].height, 0, 0);
				}

				for (int i = 0; i < 4; ++i) 
				{
					PixelShaderManager::SetColorChangedCustom(0, i, Rasterizer::tev.Reg[i][Tev::RED_C],
											Rasterizer::tev.Reg[i][Tev::ALP_C],
											Rasterizer::tev.Reg[i][Tev::BLU_C],
											Rasterizer::tev.Reg[i][Tev::GRN_C]);
					PixelShaderManager::SetColorChangedCustom(1, i, Rasterizer::tev.KonstantColors[i][Tev::RED_C],
											Rasterizer::tev.KonstantColors[i][Tev::ALP_C],
											Rasterizer::tev.KonstantColors[i][Tev::BLU_C],
											Rasterizer::tev.KonstantColors[i][Tev::GRN_C]);
				}

				glBindBuffer(GL_UNIFORM_BUFFER, s_uniformBuffer->m_buffer);
				auto buffer = s_uniformBuffer->Map(sizeof(PixelShaderConstants), 0);
				memcpy(buffer.first, &PixelShaderManager::constants, sizeof(PixelShaderConstants));
				s_uniformBuffer->Unmap(sizeof(PixelShaderConstants));
				glBindBufferRange(GL_UNIFORM_BUFFER, 1, s_uniformBuffer->m_buffer, buffer.second, sizeof(PixelShaderConstants));
			}

			int offset = 0;
			glEnableVertexAttribArray(SHADER_POSITION_ATTRIB);
			glVertexAttribPointer(SHADER_POSITION_ATTRIB, 3, GL_FLOAT, true, vertex_stride, (u8*)nullptr);
			offset += 3 * sizeof(float);

			glDisableVertexAttribArray(SHADER_TEXTURE0_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE1_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE2_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE3_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE4_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE5_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE6_ATTRIB);
			glDisableVertexAttribArray(SHADER_TEXTURE7_ATTRIB);
			glDisableVertexAttribArray(SHADER_COLOR0_ATTRIB);
//			if (hasColor)
			{
				glEnableVertexAttribArray(SHADER_COLOR0_ATTRIB);
				glVertexAttribPointer(SHADER_COLOR0_ATTRIB, 4, GL_FLOAT, true, vertex_stride, (u8*)nullptr + offset);
				offset += 4 * sizeof(float);
			}
			for (int tc = 0; tc < 8; ++tc)
				if (has_texcoord[tc])
				{
					glEnableVertexAttribArray(SHADER_TEXTURE0_ATTRIB + tc);
					glVertexAttribPointer(SHADER_TEXTURE0_ATTRIB + tc, 2, GL_FLOAT, true, vertex_stride, (u8*)nullptr + offset);
					offset += 2 * sizeof(float);
				}

			glUseProgram(programID);
			glBindBuffer(GL_ARRAY_BUFFER, s_vertexBuffer->m_buffer);
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, s_indexBuffer->m_buffer);
			glBindBuffer(GL_UNIFORM_BUFFER, s_uniformBuffer->m_buffer);
			glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_SHORT, (u8*)nullptr);
			glBindBuffer(GL_ARRAY_BUFFER, 0);
			glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
			glBindBuffer(GL_UNIFORM_BUFFER, 0);
			GL_REPORT_ERRORD();

			glDeleteVertexArrays(1, &VAO);
//			glDeleteProgram(programID); // TODO: Free those!
		}
		GL_REPORT_ERRORD();

	}

	void Clear()
	{
		u8 r = (bpmem.clearcolorAR & 0x00ff);
		u8 g = (bpmem.clearcolorGB & 0xff00) >> 8;
		u8 b = (bpmem.clearcolorGB & 0x00ff);
		u8 a = (bpmem.clearcolorAR & 0xff00) >> 8;

		GLfloat left   = (GLfloat)bpmem.copyTexSrcXY.x / efbHalfWidth - 1.0f;
		GLfloat top    = 1.0f - (GLfloat)bpmem.copyTexSrcXY.y / efbHalfHeight;
		GLfloat right  = (GLfloat)(left + bpmem.copyTexSrcWH.x + 1) / efbHalfWidth - 1.0f;
		GLfloat bottom = 1.0f - (GLfloat)(top + bpmem.copyTexSrcWH.y + 1) / efbHalfHeight;
		GLfloat depth = (GLfloat)bpmem.clearZValue / (GLfloat)0x00ffffff;
		const GLfloat verts[4][3] = {
			{ left, top, depth },
			{ right, top, depth },
			{ right, bottom, depth },
			{ left, bottom, depth }
		};
		{
			GLboolean const
				color_mask = bpmem.blendmode.colorupdate ? GL_TRUE : GL_FALSE,
				alpha_mask = bpmem.blendmode.alphaupdate ? GL_TRUE : GL_FALSE;
			glColorMask(color_mask,  color_mask,  color_mask,  alpha_mask);
			glDepthMask(bpmem.zmode.updateenable ? GL_TRUE : GL_FALSE);

			glUseProgram(clearProg);
			glVertexAttribPointer(clear_apos, 3, GL_FLOAT, GL_FALSE, 0, verts);
			glUniform4f(clear_ucol, r, g, b, a);
			glEnableVertexAttribArray(col_apos);
				glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
			glDisableVertexAttribArray(col_apos);
		}
		GL_REPORT_ERRORD();
	}

	TexCacheEntry::TexCacheEntry()
	{
		Create();
	}

	void TexCacheEntry::Create()
	{
		FourTexUnits &texUnit = bpmem.tex[0];

		texImage0.hex = texUnit.texImage0[0].hex;
		texImage1.hex = texUnit.texImage1[0].hex;
		texImage2.hex = texUnit.texImage2[0].hex;
		texImage3.hex = texUnit.texImage3[0].hex;
		texTlut.hex = texUnit.texTlut[0].hex;

		int image_width = texImage0.width;
		int image_height = texImage0.height;

		DebugUtil::GetTextureRGBA(temp, 0, 0, image_width, image_height);

		glGenTextures(1, (GLuint *)&texture);
		glBindTexture(texType, texture);
		glTexImage2D(texType, 0, GL_RGBA, (GLsizei)image_width, (GLsizei)image_height, 0, GL_RGBA, GL_UNSIGNED_BYTE, temp);

		GL_REPORT_ERRORD();
	}

	void TexCacheEntry::Destroy()
	{
		if (texture == 0)
			return;

		glDeleteTextures(1, &texture);
		texture = 0;
	}

	void TexCacheEntry::Update()
	{
		FourTexUnits &texUnit = bpmem.tex[0];

		// extra checks cause textures to be reloaded much more
		if (texUnit.texImage0[0].hex != texImage0.hex ||
		//	texUnit.texImage1[0].hex != texImage1.hex ||
		//	texUnit.texImage2[0].hex != texImage2.hex ||
			texUnit.texImage3[0].hex != texImage3.hex ||
			texUnit.texTlut[0].hex   != texTlut.hex)
		{
			Destroy();
			Create();
		}
	}
}

