/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.owl2java.config;

import cz.cvut.kbss.jopa.owl2java.cli.CliParams;
import cz.cvut.kbss.jopa.owl2java.cli.CommandParserProvider;
import joptsimple.OptionSet;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNull;

public class TransformationConfigurationTest {

    @Test
    public void configIgnoresContextWhenWholeOntologyUsageIsConfigured() {
        final OptionSet optionSet = CommandParserProvider.getCommandTransform().parse("-i", "-c", "test");
        final TransformationConfiguration config = TransformationConfiguration.config(new CliParams(optionSet));
        assertNull(config.getContext());
    }
}
