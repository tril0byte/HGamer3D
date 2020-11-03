//	HGamer3D Library (A project to enable 3D game development in Haskell)
//	Copyright 2015 - 2018 Peter Althainz
//
//	Distributed under the Apache License, Version 2.0
//	(See attached file LICENSE or copy at
//	http://www.apache.org/licenses/LICENSE-2.0)
//
//	file: HGamer3D/gamegio-library/src/gamegio/EntityIdCbor.cpp

#include "EntityIdCbor.hpp"

namespace cbd {

void readEntityId(CborValue *it, EntityId *entityId)
{
    std::vector<uint8_t> rval;
    { size_t l; cbor_value_calculate_string_length(it, &l); rval.resize(l+1);
        cbor_value_copy_byte_string(it, rval.data(), &l, NULL); cbor_value_advance(it);}
    *entityId = rval;
}

void writeEntityId(CborEncoder *enc, EntityId entityId)
{
    cbor_encode_byte_string(enc, entityId.data(), entityId.size());
}

void printEID(EntityId eid)
{
  for(int j = 0; j < 16; j++)
    printf("%02X", eid[j]);
  std::cout << "\n";
}

cbd::EntityId BufferToEntityId(PODVector<unsigned char> buf)
{
    cbd::EntityId eid;
    PODVector<unsigned char>::Iterator element;
    for (element = buf.Begin(); element != buf.End(); element++)
    {
      eid.push_back(*element); //cout << *element;
    }
    return eid;
}

PODVector<unsigned char> EntityIdToBuffer(cbd::EntityId eid)
{
  PODVector<unsigned char> buf; //construct Urho3D buffer
  std::vector<uint8_t>::iterator index=eid.begin();
  for (int i=0; i<16; i++)
  {
    buf.Push(*index++);
  }
  return buf;
}

} // end of namespacd cdb

const uint64_t ctEntityId = 0x112cc0dc2647d39e;
