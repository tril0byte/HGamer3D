// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// ClassMouse.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMouse
#define _DEFINED_HG3D_ClassMouse

#include "ClassPtr.h"
#include "EnumMouseButton.h"


// Check if a mouse button is pressed. 
void sfml_mse_isButtonPressed(enum EnumMouseButton button_c, int * result_c);

#endif 