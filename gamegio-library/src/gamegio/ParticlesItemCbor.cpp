#include <stdint.h>
#include <stdbool.h>
#include <string>
#include <vector>
#include "cbor.h"
#include "cborconstants_p.h"
#include "ParticlesItemCbor.hpp"

namespace cbd {

void readParticles(CborValue *it, Particles *particles) {
{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);
int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);
    particles->selector = (EnumParticles)i;
    if (particles->selector == 0) {
        { size_t l; cbor_value_calculate_string_length(it, &l); particles->data.ParticleEffectResource.value0.resize(l+1);
        cbor_value_copy_text_string(it, (char *)(particles->data.ParticleEffectResource.value0.c_str()), &l, NULL); cbor_value_advance(it);}
    };
cbor_value_leave_container(ita, it); }
}

void writeParticles(CborEncoder *enc, Particles particles) {
    if (particles.selector == 0) 
{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, 2);
        cbor_encode_uint(enc, (uint64_t)particles.selector);
        cbor_encode_text_stringz(enc, particles.data.ParticleEffectResource.value0.c_str());
cbor_encoder_close_container_checked(enca, enc); }
}


} // end of namespacd cdb

const uint64_t ctParticles = 0x5009dcc85ea5f959;
