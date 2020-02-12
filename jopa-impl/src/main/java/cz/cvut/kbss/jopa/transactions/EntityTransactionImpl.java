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
        wrapper.begin();
        this.active = true;
        wrapper.getEntityManager().transactionStarted(this);
        LOG.trace("EntityTransaction begin.");
    }

    @Override
    public void commit() {
        verifyTransactionActive("commit");
        try {
            LOG.trace("EntityTransaction commit started.");
            if (rollbackOnly) {
                throw new RollbackException("Trying to commit transaction marked as rollback only.");
            } else {
                try {
                    wrapper.getTransactionUOW().commit();
                } catch (RuntimeException ex) {
                    wrapper.getEntityManager().removeCurrentPersistenceContext();
                    throw new RollbackException(ex);
                }
            }
        } finally {
            if (wrapper.getTransactionUOW().shouldReleaseAfterCommit()) {
                wrapper.getEntityManager().removeCurrentPersistenceContext();
            }
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
        wrapper.setTransactionUOW(null);
        wrapper.getEntityManager().transactionFinished(this);
    }

    @Override
    public void rollback() {
        verifyTransactionActive("rollback");
        wrapper.getTransactionUOW().rollback();
        wrapper.getEntityManager().removeCurrentPersistenceContext();
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
