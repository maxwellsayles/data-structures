/**
 * @file hash_table.h
 * 
 * Originally written by Andrew V. Sutherland 2007-2008.
 * Rewritten and heavily modified by Maxwell Sayles, 2009-2010.
 *
 * This code is meant to provide simple hash table insertion/lookup.  This is
 * designed for small tables that fit into cache memory, using 64 bits per
 * entry (96 bits per overflow entry).  The table is optimized for
 * unsuccessful lookups, which are expected to be typical during a BSGS
 * search.
 *
 * Each table entry is a pair of 32-bit values, a key/value pair.  The value
 * can be any NON-ZERO 32-bit value.
 */
#pragma once
#ifndef LINKED_HASH__INCLUDED
#define LINKED_HASH__INCLUDED

#include <stdint.h>

struct htab_entry;
struct htab_list_item;

typedef struct {
  uint8_t* block;  // block of contiguous memory
  int block_size;  // size in bytes of block
    
  struct htab_entry* primary;
  int primary_entries;
  uint32_t primary_mask;
    
  struct htab_list_item* lists;
  int list_next;
  int list_entries;

#ifdef _DEBUG
  uint64_t insert_count;
  uint64_t collision_count;
  uint64_t deep_collision_count;
#endif
} hash_table_t;

/// Initialize a hash table with the given sizes.
/// primary_entries will be rounded up to the nearest power of 2
/// overflow_entries must be >= primary_entries after rounding up.
void hash_table_init(hash_table_t* hash,
                     int primary_entries,
                     int overflow_entries);

/// Release the memory for a given hash table.
void hash_table_clear(hash_table_t* hash);

/// Reset a hash table to its initial state (i.e. no members).
/// primary_entries will be rounded up to the nearest power of 2
void hash_table_reset(hash_table_t* hash, int primary_entries);

/// Insert an element into a table.
/// IMPORTANT: datum must be non-zero.
void hash_table_insert(hash_table_t* hash, uint32_t datum, uint32_t key);

/// Returns the number of entries placed in data.
/// data_size is assumed to be >= 2. There is no error checking done.
int hash_table_lookup(hash_table_t* hash,
                      uint32_t* data, int data_size, uint32_t key);

/// Does a combined insert/lookup,
/// returning a list of data for any previously inserted entries
/// with the same key value. The returned int is the number of entries.
/// data_size is assumed to be >= 2. There is no error checking done.
int hash_table_insert_matches(hash_table_t* hash,
                              uint32_t* data, int data_size,
                              uint32_t datum, uint32_t key);

#endif

