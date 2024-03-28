(*---------------------------------------------------------------------------
   Copyright (c) 2024 The omod programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let name cu = match cu.Cmo_format.cu_name with
| Cmo_format.Compunit name -> name
