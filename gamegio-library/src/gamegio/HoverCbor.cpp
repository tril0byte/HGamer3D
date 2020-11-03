#include "HoverCbor.hpp"

namespace cbd {

void readHoverEvent(CborValue *it0, HoverEvent *hoverEvent)
{
  CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
  int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
  hoverEvent->selector = (EnumMaybeEntity)i;
  if (hoverEvent->selector == 0) {
  };
  if (hoverEvent->selector == 1) {
    readEntityId(it, &(hoverEvent->data.HoverEntity.value0));
  };
  cbor_value_leave_container(it0, it);
}


void writeHoverEvent(CborEncoder *enc0, HoverEvent hoverEvent)
{
  if (hoverEvent.selector == 0) {
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
    cbor_encode_uint(enc, (uint64_t)hoverEvent.selector);
    cbor_encoder_close_container_checked(enc0, enc);
  };
  if (hoverEvent.selector == 1) {
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 2);
    cbor_encode_uint(enc, (uint64_t)hoverEvent.selector);
    writeEntityId(enc, hoverEvent.data.HoverEntity.value0);
    cbor_encoder_close_container_checked(enc0, enc);
  };
}

} // end of namespace cdb

const uint64_t ctHoverCamera = 0xc9947e814b1f9b89;
const uint64_t ctHoverEvent = 0x76492fa6aa357311;
