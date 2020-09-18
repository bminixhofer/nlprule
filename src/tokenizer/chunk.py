import onnxruntime

session = onnxruntime.InferenceSession("src/tokenizer/train-chunker/model.onnx")


def predict(input_ids, attention_mask):
    return session.run(
        None, {"input_ids": input_ids, "attention_mask": attention_mask}
    )[0].tolist()
