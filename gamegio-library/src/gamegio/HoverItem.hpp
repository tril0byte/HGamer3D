// Mouse Hover over entities in 3D for HGamer3D
//
// This file has been released by its author to the public domain
// For details, see https://creativecommons.org/publicdomain/zero/1.0/ 
//
// file: HGamer3D/gamegio-library/src/gamegio/Hover.hpp

#ifndef __hover_hpp__
#define __hover_hpp__

#include "Urho3D/Urho3D.h"

#include "Fresco.hpp"
#include "Graphics3DSystem.hpp"
#include "EntityIdCbor.hpp"
#include "HoverCbor.hpp"

#define HOVER_ENTITY_HASHKEY "EntityId"

using namespace Urho3D;

// declares extern C wrapper function for C++ method
GIO_METHOD_DEC(HoverItem, HoverCamera)
GIO_METHOD_DEC(HoverItem, HoverEvent)
GIO_METHOD_DEC(HoverItem, HoverMode)
GIO_METHOD_DEC(HoverItem, DragVector)
// declares class CNAMEFactory
GCO_FACTORY_DEC(HoverItem)

class HoverItem : public Object {

URHO3D_OBJECT(HoverItem, Object);

private:
  cbd::EntityId hoverEntity; // current EID hovered over
  Camera* camera; // the screen we are looking from. required for hover and drag
  Vector3 hoverHitPosition; // constantly update during Hovering mode
  cbd::EnumHoverMode hoverMode; // default is Hovering. May also be dragging.
  Vector3 dragStartPosition; // cached upon start of dragging.

  // interface for Fresco callback
  FrMessageFn2 hoverEventF;
  void* hoverDataP;
  uint64_t hoverEventType;

  bool CompareEntities(cbd::EntityId* a, cbd::EntityId* b);
  void HandleHovering(StringHash eventType, VariantMap& eventData);
  void HandleDragging(StringHash eventType, VariantMap& eventData);

public:
  HoverItem();
  ~HoverItem();
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgHoverCamera(FrMsg m, FrMsgLength l);
  void msgHoverEvent(FrMsg m, FrMsgLength l);
  void msgHoverMode(FrMsg m, FrMsgLength l);
  void msgDragVector(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void registerHoverEventFunction(FrMessageFn2 f, void* p2, uint64_t hoverET);
  void HandleMouseMove(StringHash eventType, VariantMap& eventData);
};

#endif
