/**
 * Copyright (C) 2011 Czech Technical University in Prague
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

package cz.cvut.kbss.owlpersistence.owlapi;

import cz.cvut.kbss.owlpersistence.model.EntityTransaction;

public class EntityTransactionImpl implements EntityTransaction {

	private boolean rollbackOnly;

	private boolean active;

	private AbstractEntityManager em;

	public EntityTransactionImpl(final AbstractEntityManager o) {
		this.em = o;
	}

	private void ensureActive() {
		if (isActive()) {
			throw new IllegalStateException();
		}
	}

	private void ensureInactive() {
		if (!isActive()) {
			throw new IllegalStateException();
		}
	}

	public void begin() {
		ensureInactive();

		// TODO
	}

	public synchronized void commit() {
		ensureActive();
		em.flush();
	}

	public synchronized boolean getRollbackOnly() {
		ensureActive();
		return rollbackOnly;
	}

	public synchronized boolean isActive() {
		return active;
	}

	public synchronized void rollback() {
		ensureActive();
		em.clear();
	}

	public synchronized void setRollbackOnly() {
		ensureActive();
		this.rollbackOnly = true;
	}
}
