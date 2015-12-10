package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Connection;

interface ConnectionListener {

    void connectionClosed(Connection connection);
}
