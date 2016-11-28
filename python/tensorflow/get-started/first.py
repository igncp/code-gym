import tensorflow as tf
import numpy as np

# The equation is: y_data = W * x_data + b

# Create 100 phony x, y data points in NumPy, y = x * 0.1 + 0.3
x_data = np.random.rand(100).astype(np.float32)
y_data = x_data * 0.1 + 0.3

# Try to find values for W and b that compute y_data = W * x_data + b
# (We know that W should be 0.1 and b 0.3, but TensorFlow will
# figure that out for us.)
W = tf.Variable(tf.random_uniform([1], -1.0, 1.0))
b = tf.Variable(tf.zeros([1]))
y = W * x_data + b

# Minimize the mean squared errors.
loss = tf.reduce_mean(tf.square(y - y_data))
optimizer = tf.train.GradientDescentOptimizer(0.5)
train = optimizer.minimize(loss)

# Before starting, initialize the variables.  We will 'run' this first.
init = tf.initialize_all_variables()

# Launch the graph.
sess = tf.Session()
sess.run(init)

# Fit the line.
for step in range(200):
    sess.run(train)
    if step % 20 == 0:
        print ""
        print "step: " + str(step)
        print "w: " + str(sess.run(W)[0])
        print "b: " + str(sess.run(b)[0])

# TF direct functions and classes:
#   tf.Variable:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/state_ops.html#Variable
#       - https://www.tensorflow.org/versions/r0.11/how_tos/variables/index.html
#   tf.random_uniform:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/constant_op.html#random_uniform
#   tf.zeros:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/constant_op.html#zeros
#   tf.square:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/math_ops.html#square
#   tf.reduce_mean:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/math_ops.html#reduce_mean
#   tf.train.GradientDescentOptimizer:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/train.html#GradientDescentOptimizer
#   tf.initialize_all_variables:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/state_ops.html#initialize_all_variables
#   tf.Session:
#       - https://www.tensorflow.org/versions/r0.11/api_docs/python/client.html#Session
