/*
 * SHA-1 in C
 * By Steve Reid <steve@edmweb.com>
 * 100% Public Domain
 */

#define SHA1_BLOCK_LENGTH       64
#define SHA1_DIGEST_LENGTH      20

typedef struct {
	uint32_t state[5];
	uint32_t count[2];  
	uint8_t buffer[SHA1_BLOCK_LENGTH];
} SHA1_CTX;
  
static void	SHA1Transform(uint32_t state[5], const uint8_t buffer[SHA1_BLOCK_LENGTH]);
static void	SHA1Init(SHA1_CTX *context);
static void	SHA1Update(SHA1_CTX *context, const uint8_t *data, size_t len);
static void	SHA1Final(SHA1_CTX *context, uint8_t digest[SHA1_DIGEST_LENGTH]);
