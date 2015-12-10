package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Connection;

interface ConnectionListener {

	/**
	 * Notification that the specified connection has been closed.
	 * 
	 * @param connection
	 *            The closing connection
	 */
	public void connectionClosed(Connection connection);
}
