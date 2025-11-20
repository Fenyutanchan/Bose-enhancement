import h5py
import numpy as np
import os
import sys

hdf5_file_names = sys.argv[1:]
for hdf5_file_name in hdf5_file_names:
    hdf5_file_basename, hdf5_file_extension = os.path.splitext(hdf5_file_name)
    try:
        hdf5_file = h5py.File(hdf5_file_name, "r")

        with open(hdf5_file_basename + ".npz", "wb") as npz_file:
            arrays_dict = {}
            for key in hdf5_file.keys():
                value = hdf5_file[key]
                if value.dtype == "O":
                    arrays_dict[key] = np.array(value, dtype=str)
                    continue
                else:
                    arrays_dict[key] = np.array(value)
                # end for if-else
            # end for key in hdf5_file.keys()
            np.savez(npz_file, **arrays_dict)
        # end with open
    except Exception:
        continue
    # end try-except
# end for hdf5_file_name
