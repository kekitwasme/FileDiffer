from flask import Flask, render_template, jsonify
import diff_logic as diff_logic
import json

app = Flask(__name__)

dir_old = 'test/test_old'
dir_new = 'test/test_new'


diff_result = diff_logic.create_folder_diff(dir_old, dir_new)


@app.route('/')
def index():
    return render_template('index.html', result=diff_result)

#Load the file strucutre stored as a json file
@app.route('/example')
def example():
    jsonDiff = json.loads(diff_result)
    #changed from json.dump to jsonify
    return jsonify(jsonDiff)  


if __name__ == '__main__':
    app.run(debug = True)
