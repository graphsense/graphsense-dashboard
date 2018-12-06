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
    return dict(version='Version 0.3.3dev')


# TEMPLATE FILTERS

@app.template_filter()
def format_duration(start_end_tuple):
    dt1 = datetime.datetime.fromtimestamp(start_end_tuple[0])
    dt2 = datetime.datetime.fromtimestamp(start_end_tuple[1])
    rd = dateutil.relativedelta.relativedelta(dt2, dt1)
    if rd.seconds == 0:
        activity = 'Single transaction'
    else:
        if rd.years > 0:
            activity = '%d years %d months %d days' % (rd.years,
                                                       rd.months,
                                                       rd.days)
        elif rd.months > 0:
            activity = '%d months %d days %d hours' % (rd.months,
                                                       rd.days,
                                                       rd.hours)
        elif rd.days > 0:
            activity = '%d days %d hours %d minutes' % (rd.days,
                                                        rd.hours,
                                                        rd.minutes)
        else:
            activity = '%d hours %d minutes %d seconds' % (rd.hours,
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


@app.template_filter()
def format_underscore(value):
    # Capitalize first letter of each word and replace "_" with " "
    words = value.split('_')
    if words[0] == 'no':
        words[0] = 'no.'
    return ' '.join(words).title()


@app.template_filter()
def format_number(value):
    return '{:,}'.format(value)


# CONTROLLERS

@app.route('/')
def index():
    statistics = storage.statistics()
    return render_template('landing_page.html', statistics=statistics)


# SEARCH-related controllers

@app.route('/query_term_suggestions')
def query_term_suggestions():
    term_fragment = request.args.get('term_fragment')
    currency = request.args.get('currency')
    max_suggestion_items = request.args.get('max_suggestion_items')
    if not max_suggestion_items.isdigit():
        raise ValueError('Invalid argument for parameter max_suggestion_items '
                         '(not a number).')
    suggestions = storage.query_term_suggestions(term_fragment,
                                                 max_suggestion_items,
                                                 currency)
    return render_template('partials/suggestion_dropdown_menu.html',
                           suggestions=suggestions)


@app.route('/search')
def search():
    term = request.args.get('query')
    currency = request.args.get('currency-selector')
    if term:
        if len(term) < 9 and term.isdigit():
            return redirect(url_for('show_block',
                                    currency=currency, height_or_hash=term))
        elif len(term) == 64:
            return redirect(url_for('show_transaction',
                                    currency=currency, hash=term))
        else:
            address = normalize_address(term)
            if address is None:
                message = 'Couldn\'t find any match for "{}".'.format(term)
                return render_template('error.html', message=message)
            else:
                return redirect(url_for('show_address',
                                        currency=currency, address=address))
    else:
        message = 'Please provide a query term.'
        return render_template('error.html', message=message)


# ADDRESS-related controllers

@app.route('/<currency>/address/<address>')
def show_address(currency, address):
    address_details = storage.address(address, currency=currency)
    if address_details is None:
        message = 'The address {} cannot be found ' \
                  'in the blockchain.'.format(address)
        return render_template('error.html', message=message)
    else:
        return render_template('detail_address.html',
                               address=address_details, currency=currency)


@app.route('/<currency>/address/<address>/transactions.json')
def retrieve_transactions(currency, address):
    transactions = storage.address_transactions(address,
                                                limit=2500, currency=currency)
    return jsonify(transactions)


@app.route('/<currency>/address/<address>/tags.json')
def retrieve_address_tags(currency, address):
    tags = storage.address_tags(address, currency=currency)
    return jsonify(tags)


@app.route('/<currency>/address/<address>/tags.csv')
def download_address_tags(currency, address):
    tags = storage.address_tags(address, currency=currency)
    output = io.StringIO()
    writer = csv.writer(output, delimiter=',',
                        quotechar='"', quoting=csv.QUOTE_ALL)
    writer.writerow(['address', 'comment', 'link', 'source'])
    for tag in tags:
        writer.writerow([tag['address'],
                         tag['tag'],
                         tag['tagUri'],
                         tag['source']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


@app.route('/<currency>/address/<address>/egonet.json')
def retrieve_address_egonet(currency, address):
    direction = request.args.get('direction')
    limit = request.args.get('limit')
    egonet = storage.address_egonet(address, currency, direction, limit)
    return jsonify(egonet)


@app.route('/<currency>/address/<address>/egonet/nodes.csv')
def download_address_egonet_nodes(currency, address):
    egonet = storage.address_egonet(address, currency,
                                    direction='all', limit=1000)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['address', 'balance', 'received'])
    for node in egonet['nodes']:
        writer.writerow([node['id'],
                         node['balance'],
                         node['received']])
    value = output.getvalue().strip('\r\n')
    return Response(value, mimetype='text/csv')


@app.route('/<currency>/address/<address>/egonet/edges.csv')
def download_address_egonet_edges(currency, address):
    egonet = storage.address_egonet(address, currency,
                                    direction='all', limit=1000)
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

@app.route('/<currency>/tx/<hash>')
def show_transaction(currency, hash):
    tx = storage.transaction(hash, currency=currency)
    if tx is None:
        message = 'The transaction {} cannot be found ' \
                  'in the blockchain.'.format(hash)
        return render_template('error.html', message=message)
    else:
        return render_template('detail_transaction.html',
                               tx=tx, currency=currency)


# BLOCK-related controllers

@app.route('/<currency>/block/<height_or_hash>')
def show_block(currency, height_or_hash):
    block = storage.block(height_or_hash, currency=currency)
    return render_template('detail_block.html',
                           block=block, currency=currency)


@app.route('/<currency>/block/<height>/transactions.json')
def retrieve_block_transactions(currency, height):
    transactions = storage.block_transactions(height, currency=currency)
    return jsonify(transactions)


# CLUSTER-related controllers

@app.route('/<currency>/cluster/<cluster_id>')
def show_cluster(currency, cluster_id):
    cluster_details = storage.cluster(cluster_id, currency=currency)
    return render_template('detail_cluster.html',
                           cluster=cluster_details, currency=currency)


@app.route('/<currency>/cluster/<cluster_id>/addresses.json')
def retrieve_cluster_addresses(currency, cluster_id):
    addresses = storage.cluster_addresses(cluster_id,
                                          limit=2500, currency=currency)
    return jsonify(addresses)


@app.route('/<currency>/cluster/<cluster_id>/tags.json')
def retrieve_cluster_tags(currency, cluster_id):
    tags = storage.cluster_tags(cluster_id, currency=currency)
    return jsonify(tags)


@app.route('/<currency>/cluster/<cluster_id>/tags.csv')
def download_cluster_tags(currency, cluster_id):
    tags = storage.cluster_tags(cluster_id, currency=currency)
    output = io.StringIO()
    writer = csv.writer(output, delimiter=',',
                        quotechar='"', quoting=csv.QUOTE_ALL)
    writer.writerow(['address', 'comment', 'link', 'source'])
    for tag in tags:
        writer.writerow([tag['address'],
                         tag['tag'],
                         tag['tagUri'],
                         tag['source']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


@app.route('/<currency>/cluster/<cluster_id>/egonet.json')
def retrieve_cluster_egonet(currency, cluster_id):
    direction = request.args.get('direction')
    limit = request.args.get('limit')
    egonet = storage.cluster_egonet(cluster_id, currency, direction, limit)

    # temporary fix eliminates self references
    edges = []
    for edge in egonet['edges']:
        if edge['source'] != edge['target']:
            edges.append(edge)
    egonet['edges'] = edges

    return jsonify(egonet)


@app.route('/<currency>/cluster/<cluster_id>/egonet/nodes.csv')
def download_cluster_egonet_nodes(currency, cluster_id):
    egonet = storage.cluster_egonet(cluster_id, currency,
                                    direction='all', limit=500)
    output = io.StringIO()
    writer = csv.writer(output)
    writer.writerow(['clusterId', 'balance', 'received'])
    for node in egonet['nodes']:
        writer.writerow([node['id'],
                         node['balance'],
                         node['received']])

    value = output.getvalue().strip('\r\n')

    return Response(value, mimetype='text/csv')


@app.route('/<currency>/cluster/<cluster_id>/egonet/edges.csv')
def download_cluster_egonet_edges(currency, cluster_id):
    egonet = storage.cluster_egonet(cluster_id, currency,
                                    direction='all', limit=500)
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
