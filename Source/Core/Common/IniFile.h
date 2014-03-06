// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <cstring>
#include <map>
#include <string>
#include <vector>

#include "Common/CommonTypes.h"
#include "Common/StringUtil.h"

struct CaseInsensitiveStringCompare
{
	bool operator() (const std::string& a, const std::string& b) const
	{
		return strcasecmp(a.c_str(), b.c_str()) < 0;
	}
};

class IniFile
{
public:
	class Section
	{
		friend class IniFile;

	public:
		Section() {}
		Section(const std::string& _name) : name(_name) {}

		bool Exists(const std::string& key) const;
		bool Delete(const std::string& key);

		void Set(const std::string& key, const std::string& newValue);
		void Set(const std::string& key, const std::string& newValue, const std::string& defaultValue);

		bool Get(const std::string& key, std::string* value, const std::string& defaultValue = NULL_STRING);

		void Set(const std::string& key, u32 newValue) {
			Set(key, StringFromFormat("0x%08x", newValue));
		}
		void Set(const std::string& key, float newValue) {
			Set(key, StringFromFormat("%f", newValue));
		}
		void Set(const std::string& key, const float newValue, const float defaultValue);
		void Set(const std::string& key, double newValue) {
			Set(key, StringFromFormat("%f", newValue));
		}

		void Set(const std::string& key, int newValue, int defaultValue);
		void Set(const std::string& key, int newValue) {
			Set(key, StringFromInt(newValue));
		}

		void Set(const std::string& key, bool newValue, bool defaultValue);
		void Set(const std::string& key, bool newValue) {
			Set(key, StringFromBool(newValue));
		}
		void Set(const std::string& key, const std::vector<std::string>& newValues);

		bool Get(const std::string& key, int* value, int defaultValue = 0);
		bool Get(const std::string& key, u32* value, u32 defaultValue = 0);
		bool Get(const std::string& key, bool* value, bool defaultValue = false);
		bool Get(const std::string& key, float* value, float defaultValue = false);
		bool Get(const std::string& key, double* value, double defaultValue = false);
		bool Get(const std::string& key, std::vector<std::string>& values);

		bool operator < (const Section& other) const {
			return name < other.name;
		}

	protected:
		std::string name;

		std::vector<std::string> keys_order;
		std::map<std::string, std::string, CaseInsensitiveStringCompare> values;

		std::vector<std::string> lines;
	};

	/**
	 * Loads sections and keys.
	 * @param filename filename of the ini file which should be loaded
	 * @param keep_current_data If true, "extends" the currently loaded list of sections and keys with the loaded data (and replaces existing entries). If false, existing data will be erased.
	 * @warning Using any other operations than "Get*" and "Exists" is untested and will behave unexpectedly
	 * @todo This really is just a hack to support having two levels of gameinis (defaults and user-specified) and should eventually be replaced with a less stupid system.
	 */
	bool Load(const std::string& filename, bool keep_current_data = false);

	bool Save(const std::string& filename);

	// Returns true if key exists in section
	bool Exists(const std::string& sectionName, const std::string& key) const;

	// TODO: Get rid of these, in favor of the Section ones.
	void Set(const std::string& sectionName, const std::string& key, const std::string& newValue) {
		GetOrCreateSection(sectionName)->Set(key, newValue);
	}
	void Set(const std::string& sectionName, const std::string& key, int newValue) {
		GetOrCreateSection(sectionName)->Set(key, newValue);
	}
	void Set(const std::string& sectionName, const std::string& key, u32 newValue) {
		GetOrCreateSection(sectionName)->Set(key, newValue);
	}
	void Set(const std::string& sectionName, const std::string& key, bool newValue) {
		GetOrCreateSection(sectionName)->Set(key, newValue);
	}
	void Set(const std::string& sectionName, const std::string& key, const std::vector<std::string>& newValues) {
		GetOrCreateSection(sectionName)->Set(key, newValues);
	}

	// TODO: Get rid of these, in favor of the Section ones.
	bool Get(const std::string& sectionName, const std::string& key, int* value, int defaultValue = 0);
	bool Get(const std::string& sectionName, const std::string& key, u32* value, u32 defaultValue = 0);
	bool Get(const std::string& sectionName, const std::string& key, bool* value, bool defaultValue = false);
	bool Get(const std::string& sectionName, const std::string& key, std::vector<std::string>& values);
	bool Get(const std::string& sectionName, const std::string& key, std::string* value, const std::string& defaultValue = NULL_STRING);

	template<typename T> bool GetIfExists(const std::string& sectionName, const std::string& key, T value)
	{
		if (Exists(sectionName, key))
			return Get(sectionName, key, value);
		return false;
	}

	bool GetKeys(const std::string& sectionName, std::vector<std::string>& keys) const;

	void SetLines(const std::string& sectionName, const std::vector<std::string> &lines);
	bool GetLines(const std::string& sectionName, std::vector<std::string>& lines, const bool remove_comments = true) const;

	bool DeleteKey(const std::string& sectionName, const std::string& key);
	bool DeleteSection(const std::string& sectionName);

	void SortSections();

	Section* GetOrCreateSection(const std::string& section);

private:
	std::vector<Section> sections;

	const Section* GetSection(const std::string& section) const;
	Section* GetSection(const std::string& section);
	std::string* GetLine(const std::string& section, const std::string& key);
	void CreateSection(const std::string& section);
	
	static const std::string& NULL_STRING;
};
