// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <string>
#include <vector>

#include "Common/CommonTypes.h"
#include "DiscIO/Volume.h"

// --- this volume type is used for Wad files ---
// Some of this code might look redundant with the CNANDContentLoader class, however,
// We do not do any decryption here, we do raw read, so things are -Faster-

namespace DiscIO
{

class IBlobReader;

class CVolumeWAD : public IVolume
{
public:
	CVolumeWAD(IBlobReader* _pReader);
	~CVolumeWAD();
	bool Read(u64 _Offset, u64 _Length, u8* _pBuffer) const override;
	bool RAWRead(u64 _Offset, u64 _Length, u8* _pBuffer) const override { return false; }
	bool GetTitleID(u8* _pBuffer) const override;
	std::string GetUniqueID() const override;
	std::string GetMakerID() const override;
	std::vector<std::string> GetNames() const override;
	u32 GetFSTSize() const override               { return 0; }
	std::string GetApploaderDate() const override { return "0"; }
	ECountry GetCountry() const override;
	u64 GetSize() const override;
	u64 GetRawSize() const override;

private:
	IBlobReader* m_pReader;
	u32 OpeningBnrOffset, hdr_size, cert_size, tick_size, tmd_size, data_size;
	u8 m_Country;
};

} // namespace
