/* C stubs for vec128 operations */

#include <stdint.h>
#include <immintrin.h>

int64_t vec128_low_int64(__m128i v)
{
  return _mm_cvtsi128_si64(v);
}

int64_t vec128_high_int64(__m128i v)
{
  return _mm_cvtsi128_si64(_mm_srli_si128(v, 8));
}

__m128i vec128_of_int64s(int64_t low, int64_t high)
{
  return _mm_set_epi64x(high, low);
}