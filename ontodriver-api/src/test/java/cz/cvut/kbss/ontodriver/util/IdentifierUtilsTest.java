/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.util;

import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

class IdentifierUtilsTest {

    private static final String URI_WITH_SLASH = "http://onto.fel.cvut.cz/ontologies/jopa/ClassA";

    @Test
    void generateIdentifierAppendsInstanceIdentifierToUriWithHashFragment() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa#IdentifierUtilsTest");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("_instance"));
    }

    @Test
    void generateIdentifierAppendsIdentifierToUriEndingWithSlash() {
        final URI clsUri = URI.create(URI_WITH_SLASH);
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }

    @Test
    void generateIdentifierAppendsIdentifierWithSlashToUriWithoutHashFragment() {
        final URI clsUri = URI.create(URI_WITH_SLASH);
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }
}
