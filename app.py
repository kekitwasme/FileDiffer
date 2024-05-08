import os
from flask import Flask, render_template, jsonify
import diff_logic as diff_logic
import json
import app_logic

app = Flask(__name__)

dir_old = 'test/test_old'
dir_new = 'test/test_new'


diff_result = diff_logic.create_folder_diff(dir_old, dir_new)


@app.route('/')
def index():
    return render_template('index.html', result=diff_result)


#Load the file strucutre stored as a json file
@app.route('/raw')
def example():
    jsonDiff = json.loads(diff_result)
    #changed from json.dump to jsonify
    return jsonify(jsonDiff)  

#Load the file strucutre stored as a json file
@app.route('/test')
def testLoad():
    new = b'b0f48bdca8920cd45c276f6a22b4e2604bd47cb7'
    old = b'67ffa3361e6ddf6680dba4da6845b7baa2be90c4'

    # Use this directory as the repo path
    this_directory = os.path.dirname(__file__)

    test = app_logic.dulwich_get_data(this_directory,new,old)

    result = app_logic.diff_arrange_data(test)

    jsonDiff = json.loads(result)
    #changed from json.dump to jsonify
    return jsonify(jsonDiff)  


if __name__ == '__main__':
    app.run(debug = True)
