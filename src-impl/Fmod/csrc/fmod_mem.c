#include <string.h> 
#include "fmod.h"

FMOD_RESULT fmod_create_sound_from_memory(
  FMOD_SYSTEM *system,
  const void  *data,
  int          length,
  FMOD_SOUND **out)
{
  FMOD_CREATESOUNDEXINFO exinfo;
  memset(&exinfo, 0, sizeof(exinfo));
  exinfo.cbsize = sizeof(exinfo);
  exinfo.length = (unsigned int)length;

  FMOD_MODE mode = FMOD_OPENMEMORY | FMOD_CREATESAMPLE;
  return FMOD_System_CreateSound(system, (const char*)data, mode, &exinfo, out);
}
