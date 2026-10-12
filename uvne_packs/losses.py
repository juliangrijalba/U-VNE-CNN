import tensorflow as tf
import tensorflow.keras.backend as K
from tensorflow.keras.utils import register_keras_serializable

@register_keras_serializable(package="Custom")
class FocalLossMultilabel(tf.keras.losses.Loss):

    def __init__(
        self,
        alpha,
        gamma=2.0,
        reduction=tf.keras.losses.Reduction.SUM_OVER_BATCH_SIZE,
        name="focal_loss_multilabel",
    ):
        super().__init__(reduction=reduction, name=name)
        self.alpha = tf.constant(alpha, dtype=tf.float32)
        self.gamma = gamma

    def call(self, y_true, y_pred_logits):
        y_pred = tf.nn.sigmoid(y_pred_logits)

        eps = K.epsilon()
        y_pred = tf.clip_by_value(y_pred, eps, 1.0 - eps)

        bce = - (
            y_true * tf.math.log(y_pred)
            + (1.0 - y_true) * tf.math.log(1.0 - y_pred)
        )

        p_t = y_true * y_pred + (1.0 - y_true) * (1.0 - y_pred)
        focal_factor = tf.pow(1.0 - p_t, self.gamma)

        alpha_factor = y_true * self.alpha + (1.0 - y_true)

        loss = alpha_factor * focal_factor * bce
        return tf.reduce_sum(loss, axis=1)

    def get_config(self):
        config = super().get_config()
        config.update({
            "alpha": self.alpha.numpy().tolist(),
            "gamma": self.gamma,
        })
        return config