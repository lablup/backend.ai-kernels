import unittest
import numpy as np
import tensorflow as tf

def get_entry_tf(t, d1, d2, batch):
    """
    Args:
        t: shape = [batch, d1, d2]
        d1: shape = [batch]
        d2: shape = [batch]
    Returns:
        o: shape = [batch], with o[i] = t[i, d1[i], d2[i]]
    """
    indices = tf.stack([tf.range(batch), d1, d2], axis=1)
    return tf.gather_nd(t, indices)
