#pragma once

#include <array>

#include "VideoCommon/PerfQueryBase.h"

namespace DX11 {

class PerfQuery : public PerfQueryBase
{
public:
	PerfQuery();
	~PerfQuery();

	void EnableQuery(PerfQueryGroup type);
	void DisableQuery(PerfQueryGroup type);
	void ResetQuery();
	u32 GetQueryResult(PerfQueryType type);
	void FlushResults();
	bool IsFlushed() const;

private:
	struct ActiveQuery
	{
		ID3D11Query* query;
		PerfQueryGroup query_type;
	};

	void WeakFlush();

	// Only use when non-empty
	void FlushOne();

	// when testing in SMS: 64 was too small, 128 was ok
	static const int PERF_QUERY_BUFFER_SIZE = 512;

	std::array<ActiveQuery, PERF_QUERY_BUFFER_SIZE> m_query_buffer;
	int m_query_read_pos;

	// TODO: sloppy
	volatile int m_query_count;
	volatile u32 m_results[PQG_NUM_MEMBERS];
};

} // namespace
