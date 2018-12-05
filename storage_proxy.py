import requests


class Storage:

    def __str__(self):
        return '<Storage> ' + self.host + ':' + str(self.port)

    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.url_base = 'http://{}:{}/'.format(host, port)
        self._session = requests.Session()

    # SEARCH data access

    def retrieve(self, url, currency='', params=None):
        r = self._session.get(self.url_base + currency + '/' + url, params=params)
        return r.json()

    def query_term_suggestions(self, term_fragment, max_suggestion_items, currency):
        return self.retrieve('search', currency, {'q': term_fragment, 'limit': max_suggestion_items})

    @staticmethod
    def enrich_with_fee(tx, coinbase):
        if coinbase:
            tx['totalValue'] = tx['totalOutput']
        else:
            tx['totalValue'] = tx['totalInput']
            tx['fee'] = {c: tx['totalInput'][c] - tx['totalOutput'][c]
                         for c in tx['totalInput']}
        return tx

    # ADDRESS data access

    def address(self, address, currency):
        res = self.retrieve('address/{}'.format(address), currency)
        if res is not None:
            res['tags'] = {'explicit': self.explicit_tags(address, currency),
                           'implicit': self.implicit_tags(address, currency)}
            res['cluster'] = self.address_cluster(address, currency)
        return res

    def address_transactions(self, address, currency, limit=500):
        params = {'limit': limit}
        return self.retrieve('address/{}/transactions'.format(address), currency, params)

    def explicit_tags(self, address, currency):
        return self.retrieve('address/{}/tags'.format(address), currency)

    def implicit_tags(self, address, currency):
        return self.retrieve('address/{}/implicitTags'.format(address), currency)

    def address_tags(self, address, currency):
        res = self.explicit_tags(address, currency)
        for t in res:
            t['type'] = 'explicit'
        implicit = self.implicit_tags(address, currency)
        for t in implicit:
            t['type'] = 'implicit'
        return res + implicit

    def address_cluster(self, address, currency):
        return self.retrieve('address/{}/cluster'.format(address), currency)

    def address_egonet(self, address, currency, direction=None, limit=None):
        if direction is None:
            direction = 'all'
        if limit is None:
            limit = '10'
        params = {'direction': direction, 'limit': limit}
        res = self.retrieve('address/{}/egonet'.format(address), currency, params)
        if direction == 'all':
            stats = self.retrieve('address/{}'.format(address), currency)
            res['nodes'][0]['received'] = stats['totalReceived']['satoshi']
            res['nodes'][0]['balance'] = (stats['totalReceived']['satoshi'] -
                                          stats['totalSpent']['satoshi'])
        return res

    # TRANSACTION data access

    def transaction(self, tx_id, currency):
        tx = self.retrieve('tx/{}'.format(tx_id), currency)
        if tx is not None:
            self.enrich_with_fee(tx, tx['coinbase'])
        return tx

    # BLOCK data access

    def block(self, height_or_hash, currency):
        return self.retrieve('block/{}'.format(height_or_hash), currency)

    def block_transactions(self, height, currency):
        txs = self.retrieve('block/{}/transactions'.format(height), currency)
        for tx in txs['txs']:
            self.enrich_with_fee(tx, tx['noInputs'] <= 0)
        return txs

    # CLUSTER data access

    def cluster(self, cluster_id, currency):
        return self.retrieve('cluster/{}'.format(cluster_id), currency)

    def cluster_addresses(self, cluster_id, currency, limit=500):
        params = {'limit': limit}
        return self.retrieve('cluster/{}/addresses'.format(cluster_id), currency, params)

    def cluster_tags(self, cluster_id, currency):
        return self.retrieve('cluster/{}/tags'.format(cluster_id), currency)

    def cluster_egonet(self, address, currency, direction=None, limit=None):
        if direction is None:
            direction = 'all'
        if limit is None:
            limit = '10'
        params = {'direction': direction, 'limit': limit}
        res = self.retrieve('cluster/{}/egonet'.format(address), currency, params)
        if direction == 'all':
            stats = self.retrieve('cluster/{}'.format(address), currency)
            res['nodes'][0]['received'] = stats['totalReceived']['satoshi']
            res['nodes'][0]['balance'] = (stats['totalReceived']['satoshi'] -
                                          stats['totalSpent']['satoshi'])
        return res

    def statistics(self):
        res = self.retrieve('')
        return res if res else dict()

