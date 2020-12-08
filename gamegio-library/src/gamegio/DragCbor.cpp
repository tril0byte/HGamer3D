#include "DragCbor.hpp"

namespace cbd {

void readDragEvent(CborValue *it0, DragEvent *dragEvent)
{
  CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);
  int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
  dragEvent->selector = (EnumDragState)i;
  if (dragEvent->selector == 0) {
  };
  if (dragEvent->selector == 1) {
    readPosition(it, &(dragEvent->data.DragPosition.value0));
  };
  cbor_value_leave_container(it0, it);
}


void writeDragEvent(CborEncoder *enc0, DragEvent dragEvent)
{
  if (dragEvent.selector == 0) {
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 1);
    cbor_encode_uint(enc, (uint64_t)dragEvent.selector);
    cbor_encoder_close_container_checked(enc0, enc);
  };
  if (dragEvent.selector == 1) {
    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, 3);
    cbor_encode_uint(enc, (uint64_t)dragEvent.selector);
    writePosition(enc, dragEvent.data.DragPosition.value0);
    cbor_encoder_close_container_checked(enc0, enc);
  };
}

} // end of namespace cdb

const uint64_t ctDragCamera = 0x87c2f05611cb2d50;
const uint64_t ctDragHitPosition = 0x3fd1f759fcea70b4;
const uint64_t ctDragEvent = 0xa855430c4f4b1212;
