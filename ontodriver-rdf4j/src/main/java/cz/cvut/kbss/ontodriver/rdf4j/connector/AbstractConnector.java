/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.ontodriver.rdf4j.connector;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.util.Transaction;

abstract class AbstractConnector implements Connector {

    protected final Transaction transaction;
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
    public void begin() throws Rdf4jDriverException {
        transaction.begin();
    }

    protected void verifyTransactionActive() {
        if (!transaction.isActive()) {
            throw new IllegalStateException();
        }
    }
}
