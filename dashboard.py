import csv
import datetime
import dateutil.relativedelta
import io
import os
from flask import (
    Flask, Response, redirect, render_template, request, url_for, jsonify
)
from storage_proxy import Storage

app = Flask(__name__)
app.secret_key = 'FLASK_SECRET_KEY'

print('\nStarting Web application...')

storage = Storage('localhost', 9000)


# CONTEXT PROCESSORS

@app.context_processor
def inject_version():
    return dict(version='Version 0.3.2')


# TEMPLATE FILTERS

@app.template_filter()
def format_duration(start_end_tuple):
    dt1 = datetime.datetime.fromtimestamp(start_end_tuple[0])
    dt2 = datetime.datetime.fromtimestamp(start_end_tuple[1])
    rd = dateutil.relativedelta.relativedelta(dt2, dt1)
    if rd.seconds == 0:
        activity = "Single transaction"
    else:
        if rd.years > 0:
            activity = "%d years %d months %d days" % (rd.years,
                                                       rd.months,
                                                       rd.days)
        elif rd.months > 0:
            activity = "%d months %d days %d hours" % (rd.months,
                                                       rd.days,
                                                       rd.hours)
        elif rd.days > 0:
            activity = "%d days %d hours %d minutes" % (rd.days,
                                                        rd.hours,
                                                        rd.minutes)
        else:
            activity = "%d hours %d minutes %d seconds" % (rd.hours,
                                                           rd.minutes,
                                                           rd.seconds)

    return activity


@app.template_filter()
def format_time(numeric_string):
    # TODO: refactor and handle in JS
    date_format = '%Y-%m-%d %H:%M:%S'
    time_as_date = datetime.datetime.utcfromtimestamp(int(numeric_string))
    return time_as_date.strftime(date_format)


@app.template_filter()
def tag_string(value):
    if len(value) == 1:
        return value[0]['tag']
    else:
        return '({} tags)'.format(len(value))

# CONTROLLERS


@app.route('/')
def index():
    return render_template('landing_page.html')


# SEARCH-related controllers

@app.route('/query_term_suggestions')
def query_term_suggestions():
    term_fragment = request.args.get('term_fragment')
    max_suggestion_items = request.args.get('max_suggestion_items')
    if not max_suggestion_items.isdigit():
        raise ValueError("Invalid argument for parameter max_suggestion_items "
                         "(not a number).")
    suggestions = storage.query_term_suggestions(term_fragment,
                                                 max_suggestion_items)
    return render_template('partials/suggestion_dropdown_menu.html',
                           suggestions=suggestions)


@app.route('/search')
def search():
    term = request.args.get('query')
    if term:
        if len(term) < 9 and term.isdigit():
            return redirect(url_for('show_block', height_or_hash=term))
        elif len(term) == 64:
            return redirect(url_for('show_transaction', hash=term))
        else:
            address = normalize_address(term)
            if address is None:
                message = 'Couldn\'t find any match for "{}".'.format(term)
                return render_template('error.html', message=message)
            else:
                return redirect(url_for('show_address', address=address))


# ADDRESS-related controllers

@app.route('/address/<address>')
def show_address(address):
    address_details = storage.address(address)
    if address_details is None:
        message = 'The address {} cannot be found ' \
                  'in the blockchain.'.format(address)
        return render_template('error.html', message=message)
    else:
        return render_template('detail_address.html', address=address_details)


@app.route('/address/<address>/transactions.json')
def retrieve_transactions(address):
    transactions = storage.address_transactions(address, limit=2500)
    return jsonify(transactions)


@app.route('/address/<address>/tags.json')
def retrieve_address_tags(address):
    tags = storage.address_tags(address)
    return jsonify(tags)


@app.route('/address/<address>/egonet.json')
def retrieve_address_egonet(address):
    direction = request.args.get('direction')
    limit = request.args.get('limit')

    egonet = storage.address_egonet(address, direction, limit)

    return jsonify(egonet)


@app.route('/address/<address>/egonet/nodes.csv')
def download_address_egonet_nodes(address):
    egonet = storage.address_egonet(address, direction='all', limit=1000)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['address', 'balance', 'received'])
    for node in egonet['nodes']:
        writer.writerow([node['id'],
                         node['balance']['satoshi'],
                         node['received']['satoshi']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


@app.route('/address/<address>/egonet/edges.csv')
def download_address_egonet_edges(address):
    egonet = storage.address_egonet(address, direction='all', limit=1000)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['source', 'target', 'transactions', 'estimatedValue'])
    for edge in egonet['edges']:
        writer.writerow([edge['source'],
                         edge['target'],
                         edge['estimatedValue']['satoshi'],
                         edge['transactions']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


def normalize_address(term):
    # TODO: refactor
    if 26 <= len(term) <= 35:
        return term
    else:
        # address = storage.single_address_by_identity_name(term)
        address = None
        return address if address is not None else None


# TRANSACTION-related controllers

@app.route('/tx/<hash>')
def show_transaction(hash):
    tx = storage.transaction(hash)
    if tx is None:
        message = 'The transaction {} cannot be found ' \
                  'in the blockchain.'.format(hash)
        return render_template('error.html', message=message)
    else:
        return render_template('detail_transaction.html', tx=tx)


# BLOCK-related controllers

@app.route('/block/<height_or_hash>')
def show_block(height_or_hash):
    block = storage.block(height_or_hash)
    return render_template('detail_block.html', block=block)


@app.route('/block/<height>/transactions.json')
def retrieve_block_transactions(height):
    transactions = storage.block_transactions(height)
    return jsonify(transactions)


# CLUSTER-related controllers

@app.route('/cluster/<cluster_id>')
def show_cluster(cluster_id):
    cluster_details = storage.cluster(cluster_id)
    return render_template('detail_cluster.html', cluster=cluster_details)


@app.route('/cluster/<cluster_id>/addresses.json')
def retrieve_cluster_addresses(cluster_id):
    addresses = storage.cluster_addresses(cluster_id, limit=2500)
    return jsonify(addresses)


@app.route('/cluster/<cluster_id>/tags.json')
def retrieve_cluster_tags(cluster_id):
    tags = storage.cluster_tags(cluster_id)
    return jsonify(tags)


@app.route('/cluster/<cluster_id>/egonet.json')
def retrieve_cluster_egonet(cluster_id):
    direction = request.args.get('direction')
    limit = request.args.get('limit')

    egonet = storage.cluster_egonet(cluster_id, direction, limit)

    # temporary fix eliminates self references
    edges = []
    for edge in egonet['edges']:
        if edge['source'] != edge['target']:
            edges.append(edge)
    egonet['edges'] = edges

    return jsonify(egonet)


@app.route('/cluster/<cluster_id>/egonet/nodes.csv')
def download_cluster_egonet_nodes(cluster_id):
    egonet = storage.cluster_egonet(cluster_id, direction='all', limit=500)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['clusterId', 'balance', 'received'])
    for node in egonet['nodes']:
        writer.writerow([node['id'],
                         node['balance']['satoshi'],
                         node['received']['satoshi']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


@app.route('/cluster/<cluster_id>/egonet/edges.csv')
def download_cluster_egonet_edges(cluster_id):
    egonet = storage.cluster_egonet(cluster_id, direction='all', limit=500)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['source', 'target', 'transactions', 'estimatedValue'])
    for edge in egonet['edges']:
        writer.writerow([edge['source'],
                         edge['target'],
                         edge['estimatedValue']['satoshi'],
                         edge['transactions']])

    value = output.getvalue().strip('\r\n')
    return Response(value, mimetype='text/csv')


if __name__ == '__main__':

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
    app.run(debug=True, processes=1, extra_files=extra_files)
