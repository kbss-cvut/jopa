/**
 * Copyright (C) 2019 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.sessions.AbstractSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;

public class UnitOfWorkImplStub extends UnitOfWorkImpl {

	private Object lastEntity;

	public UnitOfWorkImplStub(AbstractSession parent) {
		super(parent);

	}

	/**
	 * This is the only method we need to override.
	 */
	@Override
	public void attributeChanged(Object entity, Field field) {
		this.lastEntity = entity;
	}

	public Object getLastEntity() {
		return lastEntity;
	}

	public void setLastEntity(Object entity) {
		this.lastEntity = entity;
	}

	@Override
	public boolean isInTransaction() {
		return true;
	}
}
