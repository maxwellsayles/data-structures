/**
 * @file hash_table.c
 */
#include "libsspar/linked_hash.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <strings.h>
#include <memory.h>

/**
 * We allocate three sections of memory:
 * 
 * (1) the hash table - 64 bits per entry (32bit key, 32bit value)
 * (2) the lookaside table - 96 bits per entry
 *     (32bit key, 32bit value, 32bit index to overflow)
 * (3) overflow lists - same as lookaside table
 *
 * In most cases, only (1) gets used, and this is what we want to keep in
 * cache.
 */
struct htab_entry {
  uint32_t key;
  uint32_t datum;
};

struct htab_list_item {
  uint32_t key;
  uint32_t datum;
  uint32_t next;  // -1 indicates end of list
};

/// Round up to the nearest power of 2.
static inline uint32_t ceil_pow2_u32(uint32_t x) {
  x --;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  x ++;
  return x;
}

void hash_table_init(hash_table_t* this,
                     int primary_entries,
                     int overflow_entries) {
  // Setup sizes.
  primary_entries = ceil_pow2_u32(primary_entries);
  assert(overflow_entries >= primary_entries);
  this->primary_entries = primary_entries;
  this->primary_mask = primary_entries - 1;

  // allocate all the memory in one contiguous block
  this->block_size =
      primary_entries * sizeof(struct htab_entry)
      + overflow_entries * sizeof(struct htab_list_item);
  this->block = (uint8_t*)malloc(this->block_size);
  assert(this->block);
  bzero(this->block, this->block_size);    

  // set the primary pointer
  this->primary = (struct htab_entry*)this->block;

  // set up the overflow
  this->lists = (struct htab_list_item*)
    &this->block[primary_entries*sizeof(struct htab_entry)];
  this->list_next = primary_entries;
  this->list_entries = overflow_entries;

#ifdef _DEBUG
  this->insert_count = 0;
  this->collision_count = 0;
  this->deep_collision_count = 0;
#endif
}

void hash_table_clear(hash_table_t* this) {
  free(this->block);
  this->block = 0;
}

void hash_table_reset(hash_table_t* this, int primary_entries) {
  bzero(this->block, this->block_size);
    
  // Round up for primary entries.
  primary_entries = ceil_pow2_u32(primary_entries);
  this->primary_entries = primary_entries;
  this->primary_mask = primary_entries - 1;
  assert(primary_entries * sizeof(struct htab_entry) <= this->block_size);
    
  // Compute the number of list entries.
  this->lists = (struct htab_list_item*)
    &this->block[primary_entries * sizeof(struct htab_entry)];
  this->list_entries =
    (this->block_size - primary_entries * sizeof(struct htab_entry))
    / sizeof(struct htab_list_item);
  this->list_next = primary_entries;

#ifdef _DEBUG
  this->insert_count = 0;
  this->collision_count = 0;
  this->deep_collision_count = 0;
#endif
}

/// NOTE: To optimize for forward sequential access we would
/// have to find the tail of the list and append to that. The current method
/// of prepending to the head seems more efficient.
static void hash_table_list_insert(hash_table_t* this,
                                   uint32_t list_index,
                                   uint32_t datum,
                                   uint32_t key) {
  const int next = this->list_next;
  assert(list_index < this->primary_entries);
  assert(next < this->list_entries);
  this->lists[next].datum = datum;
  this->lists[next].key = key;
  this->lists[next].next = this->lists[list_index].next;
  this->lists[list_index].next = next;
  this->list_next ++;
}

/// IMPORTANT: datum must be non-zero.
void hash_table_insert(hash_table_t* this, uint32_t datum, uint32_t key) {
  assert(datum != 0);
  const uint32_t index = key & this->primary_mask;

#ifdef _DEBUG
  this->insert_count ++;
#endif

  if (this->primary[index].datum == 0) {
    this->primary[index].key = key;
    this->primary[index].datum = datum;
    return;
  }

  if (this->lists[index].datum == 0) {
#ifdef _DEBUG
    this->collision_count ++;
#endif
    this->lists[index].key = key;
    this->lists[index].datum = datum;
    this->lists[index].next = -1;
    return;
  }
    
  // This should be quite rare; it requires a collision of three keys.
#ifdef _DEBUG
  this->deep_collision_count ++;
#endif
  hash_table_list_insert(this, index, datum, key);
}

static int hash_table_list_matches(hash_table_t* this,
                                   uint32_t index,
                                   uint32_t* data,
                                   int data_size,
                                   uint32_t key) {
  assert(index >= this->primary_entries);
  assert(index < this->list_entries);
  int i = 0;
  do {
    if (this->lists[index].key == key) {
      assert(i < data_size);
      data[i++] = this->lists[index].datum;
    }
    index = this->lists[index].next;
  } while (index != -1);
  return i;
}

int hash_table_lookup(hash_table_t* this,
                      uint32_t* data, int data_size,
                      uint32_t key) {
  const uint32_t index = key & this->primary_mask;
  int i = 0;

  if (this->primary[index].datum == 0)
    return 0;

  if (this->primary[index].key == key)
    data[i++] = this->primary[index].datum;

  if (this->lists[index].datum == 0)
    return i;

  if (this->lists[index].key == key)
    data[i++] = this->lists[index].datum;

  if (this->lists[index].next == -1)
    return i;
    
  // this should be quite rare
  return i + hash_table_list_matches(this,
				     this->lists[index].next,
				     &data[i], data_size-i,
				     key);
}

/// does a combined insert/lookup,
/// returning a list of data for any previously
/// inserted entries with the same key value
int hash_table_insert_matches(hash_table_t* this,
                              uint32_t* data, int data_size,
                              uint32_t datum, uint32_t key) {
  const uint32_t index = key & this->primary_mask;
  int i = 0;

  if (this->primary[index].datum == 0) {
    this->primary[index].key = key;
    this->primary[index].datum = datum;
    return 0;
  }

  if (this->primary[index].key == key)
    data[i++] = this->primary[index].datum;

  if (this->lists[index].datum == 0) {
    this->lists[index].key = key;
    this->lists[index].datum = datum;
    this->lists[index].next = -1;
    return i;
  }

  if (this->lists[index].key == key)
    data[i++] = this->lists[index].datum;

  if (this->lists[index].next != -1)
    i += hash_table_list_matches(this,
				 this->lists[index].next,
				 &data[i], data_size-i,
				 key);

  hash_table_list_insert(this, index, datum, key);
  return i;
}

