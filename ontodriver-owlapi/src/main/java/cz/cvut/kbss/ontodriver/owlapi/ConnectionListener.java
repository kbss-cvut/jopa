package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver_new.Connection;

interface ConnectionListener {

    void connectionClosed(Connection connection);
}
