import argparse
import json
import struct
import zipfile
from io import BytesIO
from xml.etree import ElementTree


# see https://stackoverflow.com/a/29526850
class DataInputStream:
    def __init__(self, stream):
        self.stream = stream

    def read_boolean(self):
        return struct.unpack("?", self.stream.read(1))[0]

    def read_byte(self):
        return struct.unpack("b", self.stream.read(1))[0]

    def read_unsigned_byte(self):
        return struct.unpack("B", self.stream.read(1))[0]

    def read_char(self):
        return chr(struct.unpack(">H", self.stream.read(2))[0])

    def read_double(self):
        return struct.unpack(">d", self.stream.read(8))[0]

    def read_float(self):
        return struct.unpack(">f", self.stream.read(4))[0]

    def read_short(self):
        return struct.unpack(">h", self.stream.read(2))[0]

    def read_unsigned_short(self):
        return struct.unpack(">H", self.stream.read(2))[0]

    def read_long(self):
        return struct.unpack(">q", self.stream.read(8))[0]

    def read_utf(self):
        utf_length = struct.unpack(">H", self.stream.read(2))[0]
        return self.stream.read(utf_length).decode("utf-8")

    def read_int(self):
        return struct.unpack(">i", self.stream.read(4))[0]


def read_model(stream):
    stream = DataInputStream(stream)

    name = stream.read_utf()
    assert name == "GIS"

    # read correction constant (not used anymore)
    stream.read_int()
    # read correction params (not used anymore)
    stream.read_double()

    outcome_labels = [stream.read_utf() for _ in range(stream.read_int())]
    outcome_patterns = [
        [int(x) for x in stream.read_utf().split(" ")] for _ in range(stream.read_int())
    ]
    pred_labels = [stream.read_utf() for _ in range(stream.read_int())]

    params = []
    for pattern in outcome_patterns:
        outcome_pattern = pattern[1:]

        for _ in range(pattern[0]):
            context_parameters = [
                stream.read_double() for _ in range(len(outcome_pattern))
            ]
            params.append(
                {"outcomes": outcome_pattern, "parameters": context_parameters}
            )

    pmap = {label: param for label, param in zip(pred_labels, params)}

    return {
        # "params": params,
        # "pred_labels": pred_labels,
        "outcome_labels": outcome_labels,
        "pmap": pmap,
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--token-model", "-t", type=str)
    parser.add_argument("--pos-model", "-p", type=str)
    parser.add_argument("--chunk-model", "-c", type=str)
    parser.add_argument("--out", "-o", type=str)

    args = parser.parse_args()

    with zipfile.ZipFile(args.token_model, "r") as f:
        token_model = read_model(BytesIO(f.read("token.model")))

    with zipfile.ZipFile(args.pos_model, "r") as f:
        pos_model = read_model(BytesIO(f.read("pos.model")))

        tagdict = {}

        tree = ElementTree.fromstring(f.read("tags.tagdict").decode("utf-8"))
        for e in tree.findall("entry"):
            children = e.findall("token")
            assert len(children) == 1

            tagdict[children[0].text] = e.attrib["tags"].split()

    with zipfile.ZipFile(args.chunk_model, "r") as f:
        chunk_model = read_model(BytesIO(f.read("chunker.model")))

    json.dump(
        {
            "token_model": {"model": token_model},
            "pos_model": {"model": pos_model, "tagdict": tagdict},
            "chunk_model": {"model": chunk_model},
        },
        open(args.out, "w"),
        indent=4,
    )
