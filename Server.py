from flask import *
import FileDiffer as fd

app = Flask(__name__)

@app.route('/')
def index():
    a = fd.Diff(r"\test\file1.txt", r".\test\file2.txt")
    return render_template('index.html', result=a)

@app.route('/example')
def example():
    jsonDiff = json.loads(fd.Diff(r"\test\file1.txt", r"\test\file2.txt"))
    return jsonDiff

if __name__ == '__main__':
    app.run(debug = True)
