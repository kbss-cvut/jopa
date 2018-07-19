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
