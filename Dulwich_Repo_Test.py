from dulwich.repo import Repo
from dulwich import diff_tree
import difflib

def get_blob_content(repo, blob_sha):
    """Retrieve the content of a blob given its SHA."""
    blob = repo.object_store[blob_sha]
    return blob.as_raw_string()

# Open your repository
repo = Repo('./')

# Get commit objects for the commits you want to compare
commit1 = repo[b'507c234c393664cb5e344f2052e625adeee55b3e']
commit2 = repo[b'bcb9853603d134830399b56c07e003a82528ec6e']

# Get tree objects for each commit
tree1 = commit1.tree
tree2 = commit2.tree

# Generate the diff for the trees
diffs = diff_tree.tree_changes(repo.object_store, tree1, tree2)

# Optionally, you can filter diffs for a specific file path
file_path = b'FileDiffer.py'
specific_diffs = [d for d in diffs if d.old.path == file_path or d.new.path == file_path]

# Compare blobs for the specific file
for diff in specific_diffs:
    old_blob_content = get_blob_content(repo, diff.old.sha) if diff.old.sha else b''
    new_blob_content = get_blob_content(repo, diff.new.sha) if diff.new.sha else b''

    # Print the contents side by side (or use a diff library to show differences)
    print("Old Blob Content:\n", old_blob_content.decode('utf-8'))
    print("\nNew Blob Content:\n", new_blob_content.decode('utf-8'))
    print("\n---\n")

# Note: This simplistic approach prints the entire content of the blobs.
# For a real diff, consider using a diff library like difflib in Python.

# Using the old_blob_content and new_blob_content from above
old_content = old_blob_content.decode('utf-8').splitlines(keepends=True)
new_content = new_blob_content.decode('utf-8').splitlines(keepends=True)

# Generate a unified diff
diff = difflib.unified_diff(old_content, new_content, fromfile="Old version", tofile="New version", n=10000)

# Print the diff
print('\n'.join(diff))

