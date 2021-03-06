// https://gmarkham.com/2018-04-12-parallel-reduction-in-opencl/
__kernel void find_min(__global const float *input, __global float *output,
                       __local float *local_cache) {

  /*Grab relevant thread & work item info */
  int id = get_global_id(0);
  int local_id = get_local_id(0);
  int local_size = get_local_size(0);

  // Copy values into local memory
  local_cache[local_id] = input[id];
  barrier(CLK_LOCAL_MEM_FENCE); // Make sure all threads are caught up before
                                // processing anything

  for (int i = local_size / 2; i > 0; i /= 2) {
    if (local_id < i) {
      local_cache[local_id] =
          min(local_cache[local_id + i], local_cache[local_id]);
    }
    barrier(CLK_LOCAL_MEM_FENCE); // Make sure all threads are done before
                                  // incrementing the loop
  }
  if (!local_id) {
    float new_val = local_cache[local_id];
    while (new_val != 0.0) {
      /*Using atomics avoids threads accessing memory at the same time */
      // Atmomic_xchg lets floats be used in atomic functions
      float old_val = atomic_xchg(
          &output[0],
          0.0); // return the value in output [0] and change it for 0.0
      new_val = atomic_xchg(
          &output[0],
          min(old_val,
              new_val)); // change output[0] for the minimum of the
                         // original output[0] value and the local value
    }
  }
}
