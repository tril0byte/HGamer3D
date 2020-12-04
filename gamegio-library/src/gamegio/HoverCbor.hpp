#ifndef __Hover_cbor__
#define __Hover_cbor__

#include "EntityIdCbor.hpp"

namespace cbd {

  typedef enum {
    NoEntity = 0,
    JustEntity = 1,
  } EnumMaybeEntity;

  typedef struct {
    EnumMaybeEntity selector;
    struct {
      struct {
        EntityId value0;
      } HoverEntity;
    } data;
  } HoverEvent;

  void readHoverEvent(CborValue *it0, HoverEvent *hoverEvent);
  void writeHoverEvent(CborEncoder *enc0, HoverEvent hoverEvent);

  typedef enum {
    Hovering = 0,
    Dragging = 1,
  } EnumHoverMode;

  /*  typedef struct {
    EnumHoverMode mode;
    } HoverMode;*/

  void readHoverMode(CborValue *it0, EnumHoverMode *hoverMode);
  void writeHoverMode(CborEncoder *enc0, EnumHoverMode hoverMode);
}

extern const uint64_t ctHoverCamera;
extern const uint64_t ctHoverEvent;
extern const uint64_t ctHoverMode;

#endif

