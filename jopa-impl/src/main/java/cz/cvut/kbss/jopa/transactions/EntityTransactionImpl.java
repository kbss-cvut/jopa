/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.transactions;

import cz.cvut.kbss.jopa.exceptions.RollbackException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

public class EntityTransactionImpl implements EntityTransaction {

    private static final Logger LOG = LoggerFactory.getLogger(EntityTransactionImpl.class);

    private boolean active = false;

    private boolean rollbackOnly = false;

    private final EntityTransactionWrapper wrapper;

    EntityTransactionImpl(EntityTransactionWrapper wrapper) {
        this.wrapper = Objects.requireNonNull(wrapper);
    }

    @Override
    public void begin() {
        if (isActive()) {
            throw new IllegalStateException("Transaction already active!");
        }
        this.active = true;
        wrapper.begin();
        LOG.trace("EntityTransaction begin.");
    }

    @Override
    public void commit() {
        verifyTransactionActive("commit");
        try {
            LOG.trace("EntityTransaction commit started.");
            if (rollbackOnly) {
                throw new RollbackException("Trying to commit transaction marked as rollback only.");
            }
            wrapper.commit();
        } finally {
            wrapper.transactionFinished();
            cleanup();
            LOG.trace("EntityTransaction commit finished.");
        }
    }

    private void verifyTransactionActive(String method) {
        if (!isActive()) {
            throw new IllegalStateException("Cannot invoke " + method + " on an inactive transaction!");
        }
    }

    private void cleanup() {
        this.active = false;
        this.rollbackOnly = false;
    }

    @Override
    public void rollback() {
        verifyTransactionActive("rollback");
        wrapper.rollback();
        wrapper.transactionFinished();
        cleanup();
        LOG.trace("EntityTransaction rolled back.");
    }

    @Override
    public void setRollbackOnly() {
        verifyTransactionActive("setRollbackOnly");
        this.rollbackOnly = true;
    }

    @Override
    public boolean isRollbackOnly() {
        verifyTransactionActive("isRollbackOnly");
        return this.rollbackOnly;
    }

    @Override
    public boolean isActive() {
        return active;
    }
}
