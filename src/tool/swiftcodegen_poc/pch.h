#pragma once

#include <filesystem>

#include "cmd_reader.h"
#include "meta_reader.h"
#include "task_group.h"
#include "text_writer.h"

// Disable "Unreferenced formal parameter", since for the POC we
// kept empty method stubs.
#pragma warning(disable : 4100)