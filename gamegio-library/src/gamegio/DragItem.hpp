// Mouse Drag entities in 3D for HGamer3D
//
// This file has been released by its author to the public domain
// For details, see https://creativecommons.org/publicdomain/zero/1.0/ 
//
// file: HGamer3D/gamegio-library/src/gamegio/DragItem.hpp

#ifndef __dragitem_hpp__

#include "Urho3D/Urho3D.h"

#include "Fresco.hpp"
#include "Graphics3DSystem.hpp"
#include "Vec3Cbor.hpp"
#include "EntityIdCbor.hpp"

using namespace Urho3D;

GIO_METHOD_DEC(DragItem, DragHitPosition)
GIO_METHOD_DEC(DragItem, DragCamera)
GIO_METHOD_DEC(DragItem, DragEvent)

GCO_FACTORY_DEC(DragItem)

class DragItem : public Object {

URHO3D_OBJECT(DragItem, Object);

private:
  Vector3 hitPosition;
  Camera* camera;

  // interface for Fresco callback
  FrMessageFn2 dragEventF;
  void* dragDataP;
  uint64_t dragEventType;

public:
  DragItem();
  ~DragItem();
  static FrItem msgCreate(FrMsg m, FrMsgLength l);
  void msgDragHitPosition(FrMsg m, FrMsgLength l);
  void msgDragCamera(FrMsg m, FrMsgLength l);
  void msgDragEvent(FrMsg m, FrMsgLength l);
  void virtual msgDestroy();

  void registerDragEventFunction(FrMessageFn2 f, void* p2, uint64_t dragET);
  void HandleMouseMove(StringHash eventType, VariantMap& eventData);

};

extern const uint64_t ctDragHitPosition;
extern const uint64_t ctDragCamera;
extern const uint64_t ctDragEvent;

#endif
