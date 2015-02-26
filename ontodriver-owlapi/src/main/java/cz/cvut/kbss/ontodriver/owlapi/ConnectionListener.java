package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver_new.Connection;

/**
 * Created by ledvima1 on 26.2.15.
 */
interface ConnectionListener {

    public void connectionClosed(Connection connection);
}
