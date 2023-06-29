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
package cz.cvut.kbss.jopa.owl2java.cli;

import joptsimple.OptionSet;

import java.util.List;

/**
 * Helper class wrapping an {@link OptionSet} and adding some custom functions.
 */
public class CliParams {

    private final OptionSet params;

    public CliParams(OptionSet params) {
        this.params = params;
    }

    public boolean is(String option) {
        return params.has(option) && (Boolean) params.valueOf(option);
    }

    public boolean is(String option, boolean defaultValue) {
        return params.has(option) ? (Boolean) params.valueOf(option) : defaultValue;
    }

    public boolean has(String option) {
        return params.has(option);
    }

    public Object valueOf(String option) {
        return params.valueOf(option);
    }

    public List<String> nonOptionArguments() {
        return (List<String>) params.nonOptionArguments();
    }

    public static CliParams empty() {
        return new CliParams(CommandParserProvider.getCommandTransform().parse(""));
    }
}
