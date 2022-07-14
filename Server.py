from flask import *
import FileDiffer as fd

app = Flask(__name__)

@app.route('/')
def index():
    diff = fd.Diff(r"test\file1.txt", r"test\file2.txt")
    return json.loads(diff)

@app.route('/example')
def example():
    return "an example"

if __name__ == '__main__':
    app.run(debug = True)
