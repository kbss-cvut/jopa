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

import java.net.URI;
import java.util.Objects;

public class IRI implements AnnotationValue {

	final String iri;

	public static IRI create(final String s) {
		return new IRI(s);
	}

	IRI(String iri) {
		this.iri = Objects.requireNonNull(iri);
	}

	public URI toURI() {
		return URI.create(iri);
	}

	public String toString() {
		return iri;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + iri.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		IRI other = (IRI) obj;
		if (!iri.equals(other.iri))
			return false;
		return true;
	}
}
