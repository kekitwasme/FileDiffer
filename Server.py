from flask import *
import FileDiffer as fd
from pathlib import Path

HERE = Path('.')

app = Flask(__name__)

@app.route('/')
def index():
    a = fd.Diff(HERE / 'test' / 'file1.txt', HERE / 'test' / 'file2.txt')
    return render_template('index.html', result=a)

@app.route('/example')
def example():
    jsonDiff = json.loads(fd.Diff(HERE / 'test' / 'file1.txt', HERE / 'test' / 'file2.txt'))
    return jsonDiff

if __name__ == '__main__':
    app.run(debug = True)
