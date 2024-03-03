from flask import *
import Diff_File_Content as fd


app = Flask(__name__)

dir_old = 'test/test_old'
dir_new = 'test/test_new'


@app.route('/')
def index():
    a = fd.create_folder_diff(dir_old, dir_new)
    return render_template('index.html', result=a)

@app.route('/example')
def example():
    jsonDiff = json.loads(fd.create_folder_diff(dir_old, dir_new))
    return jsonDiff

if __name__ == '__main__':
    app.run(debug = True)
