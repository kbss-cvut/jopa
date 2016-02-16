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

package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.sessions.ConfigurationHolder;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.transactions.EntityTransaction;

public abstract class AbstractEntityManager implements EntityManager, ConfigurationHolder {

	public abstract boolean isLoaded(final Object object, final String attributeName);

	/**
	 * Return the UnitOfWork that holds the current persistence context.
	 * 
	 * @return UnitOfWork
	 */
	public abstract UnitOfWork getCurrentPersistenceContext();

	/**
	 * Remove the current persistence context UnitOfWork.
	 */
	public abstract void removeCurrentPersistenceContext();

	/**
	 * Let the managing server session know that a transaction has been started.
	 * 
	 * @param t
	 *            The entity transaction that was started.
	 */
	@NonJPA
	public abstract void transactionStarted(EntityTransaction t);

	/**
	 * Let the managing server session know that a transaction has finished
	 * successfully.
	 * 
	 * @param t
	 *            The committed entity transaction.
	 */
	@NonJPA
	public abstract void transactionFinished(EntityTransaction t);
}
