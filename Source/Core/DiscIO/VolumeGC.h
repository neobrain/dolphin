// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <string>
#include <vector>

#include "Common/CommonTypes.h"
#include "DiscIO/Volume.h"

// --- this volume type is used for GC disc images ---

namespace DiscIO
{

class IBlobReader;

class CVolumeGC : public IVolume
{
public:
	CVolumeGC(IBlobReader* _pReader);
	~CVolumeGC();
	bool Read(u64 _Offset, u64 _Length, u8* _pBuffer) const override;
	bool RAWRead(u64 _Offset, u64 _Length, u8* _pBuffer) const override;
	std::string GetUniqueID() const override;
	std::string GetRevisionSpecificUniqueID() const override;
	std::string GetMakerID() const override;
	int GetRevision() const override;
	std::vector<std::string> GetNames() const override;
	u32 GetFSTSize() const override;
	std::string GetApploaderDate() const override;
	ECountry GetCountry() const override;
	u64 GetSize() const override;
	u64 GetRawSize() const override;
	bool IsDiscTwo() const override;

	typedef std::string(*StringDecoder)(const std::string&);

	static StringDecoder GetStringDecoder(ECountry country);

private:
	IBlobReader* m_pReader;
};

} // namespace
