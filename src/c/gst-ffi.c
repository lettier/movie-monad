/*
  Movie Monad
  (C) 2017 David lettier
  lettier.com
*/

#include <gst/gst.h>
#include "gst-ffi.h"

GstTagList* get_text_tag_list(GstElement* playbin, int streamId) {
  GstTagList* tags;
  g_signal_emit_by_name(G_OBJECT(playbin), "get-text-tags", streamId, &tags);
  return tags;
}
