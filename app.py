from flask import Flask, render_template, jsonify
import diff_logic as diff_logic
import json

app = Flask(__name__)

git_dir = "."
diff_target = "HEAD^"
diff_source = None
diff_result = diff_logic.GenerateDiff(".").output_merge_diff_json(diff_target, diff_source)

@app.route('/')
def index():
    return render_template('index.html', result=diff_result)


#Load the file strucutre stored as a json file
@app.route('/raw')
def example():
    jsonDiff = json.loads(diff_result)
    #changed from json.dump to jsonify
    return jsonify(jsonDiff)  


if __name__ == '__main__':
    app.run(debug = True)
