from dashboard import app as application
import os

if __name__ == "__main__":
    extra_dirs = ['templates', 'static']
    extra_files = extra_dirs[:]
    for extra_dir in extra_dirs:
        for dirname, dirs, files in os.walk(extra_dir):
            for filename in files:
                filename = os.path.join(dirname, filename)
                if os.path.isfile(filename):
                    extra_files.append(filename)

    # deactivate debug and multiple processes in production
    # because of memory usage and security
    # host='0.0.0.0',port='8888'
    application.run(debug=False, processes=1, extra_files=extra_files)
