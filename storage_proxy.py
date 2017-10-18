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

    def retrieve(self, url, params=None):
        r = self._session.get(self.url_base + url, params=params)
        return r.json()

    def query_term_suggestions(self, term_fragment, limit):
        return self.retrieve('search', {'q': term_fragment, 'limit': limit})

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

    def address(self, address):
        res = self.retrieve('address/{}'.format(address))
        if res is not None:
            res['tags'] = {'explicit': self.explicit_tags(address),
                           'implicit': self.implicit_tags(address)}
            res['cluster'] = self.address_cluster(address)
        return res

    def address_transactions(self, address, limit=500):
        params = {'limit': limit}
        return self.retrieve('address/{}/transactions'.format(address), params)

    def explicit_tags(self, address):
        return self.retrieve('address/{}/tags'.format(address))

    def implicit_tags(self, address):
        return self.retrieve('address/{}/implicitTags'.format(address))

    def address_tags(self, address):
        res = self.explicit_tags(address)
        for t in res:
            t['type'] = 'explicit'
        implicit = self.implicit_tags(address)
        for t in implicit:
            t['type'] = 'implicit'
        return res + implicit

    def address_cluster(self, address):
        return self.retrieve('address/{}/cluster'.format(address))

    def address_egonet(self, address, direction=None, limit=None):
        if direction is None:
            direction = 'all'
        if limit is None:
            limit = '10'
        params = {'direction': direction, 'limit': limit}
        res = self.retrieve('address/{}/egonet'.format(address), params)
        if direction == 'all':
            stats = self.retrieve('address/{}'.format(address))
            res['nodes'][0]['received'] = stats['totalReceived']['satoshi']
            res['nodes'][0]['balance'] = (stats['totalReceived']['satoshi'] -
                                          stats['totalSpent']['satoshi'])
        return res

    # TRANSACTION data access

    def transaction(self, tx_id):
        tx = self.retrieve('tx/{}'.format(tx_id))
        if tx is not None:
            self.enrich_with_fee(tx, tx['coinbase'])
        return tx

    # BLOCK data access

    def block(self, height_or_hash):
        return self.retrieve('block/{}'.format(height_or_hash))

    def block_transactions(self, height):
        txs = self.retrieve('block/{}/transactions'.format(height))
        for tx in txs['txs']:
            self.enrich_with_fee(tx, tx['noInputs'] <= 0)
        return txs

    # CLUSTER data access

    def cluster(self, cluster_id):
        return self.retrieve('cluster/{}'.format(cluster_id))

    def cluster_addresses(self, cluster_id, limit=500):
        params = {'limit': limit}
        return self.retrieve('cluster/{}/addresses'.format(cluster_id), params)

    def cluster_tags(self, cluster_id):
        return self.retrieve('cluster/{}/tags'.format(cluster_id))

    def cluster_egonet(self, address, direction=None, limit=None):
        if direction is None:
            direction = 'all'
        if limit is None:
            limit = '10'
        params = {'direction': direction, 'limit': limit}
        res = self.retrieve('cluster/{}/egonet'.format(address), params)
        if direction == 'all':
            stats = self.retrieve('cluster/{}'.format(address))
            res['nodes'][0]['received'] = stats['totalReceived']['satoshi']
            res['nodes'][0]['balance'] = (stats['totalReceived']['satoshi'] -
                                          stats['totalSpent']['satoshi'])
        return res
