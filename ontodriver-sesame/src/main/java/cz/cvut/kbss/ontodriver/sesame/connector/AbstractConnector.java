/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.util.Transaction;

abstract class AbstractConnector implements Connector {

    protected Transaction transaction;
    protected boolean open;

    protected AbstractConnector() {
        this.transaction = new Transaction();
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        this.open = false;
    }

    @Override
    public void begin() throws SesameDriverException {
        transaction.begin();
    }

    protected void verifyTransactionActive() {
        if (!transaction.isActive()) {
            throw new IllegalStateException();
        }
    }
}
