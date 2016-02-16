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
package cz.cvut.kbss.jopa.query.parameter;

import java.net.URI;

/**
 * Parameter value that will be put as an IRI into the query. I.e. it will be enclosed in < and >.
 */
class UriParameterValue extends ParameterValue {

    private final URI uri;

    public UriParameterValue(URI uri) {
        this.uri = uri;
    }

    @Override
    public URI getValue() {
        return uri;
    }

    @Override
    public String getQueryString() {
        return "<" + uri.toString() + ">";
    }

    @Override
    public String toString() {
        return getQueryString();
    }
}
