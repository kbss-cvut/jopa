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
package cz.cvut.kbss.ontodriver.util;

import org.junit.Test;

import java.net.URI;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

public class IdentifierUtilsTest {

    @Test
    public void generateIdentifierAppendsInstanceIdentifierToUriWithHashFragment() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa#IdentifierUtilsTest");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("_instance"));
    }

    @Test
    public void generateIdentifierAppendsIdentifierToUriEndingWithSlash() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ClassA/");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }

    @Test
    public void generateIdentifierAppendsIdentifierWithSlashToUriWithoutHashFragment() {
        final URI clsUri = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/ClassA");
        final URI result = IdentifierUtils.generateIdentifier(clsUri);
        assertThat(result.toString(), containsString("/instance"));
    }
}