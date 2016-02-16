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
package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

/**
 * This interface declares the basic methods for working with sequences in JOPA.
 * @author kidney
 *
 */
public interface ListDescriptor {

	/**
	 * Gets context in which the list is stored.
	 * 
	 * @return Context URI
	 */
	public abstract URI getContext();

	/**
	 * Sets context of the list.
	 * 
	 * @param context
	 *            Context URI, can be {@code null}
	 */
	public abstract void setContext(URI context);

	/**
	 * Gets owner of the list. </p>
	 * 
	 * That is, the named resource which is at the head of the list. In object
	 * model, it is the owning entity.
	 * 
	 * @return List owner
	 */
	public abstract NamedResource getListOwner();

	/**
	 * Gets the property assertion which connects the list to its owner.
	 * 
	 * @return Property assertion
	 * @see #getListOwner()
	 */
	public abstract Assertion getListProperty();

	/**
	 * Gets the property assertion which connects the list nodes to each other.
	 * 
	 * @return Property assertion
	 */
	public abstract Assertion getNextNode();

}