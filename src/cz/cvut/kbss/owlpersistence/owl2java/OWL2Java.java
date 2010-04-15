package cz.cvut.kbss.owlpersistence.owl2java;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import joptsimple.OptionParser;
import joptsimple.OptionSet;

public class OWL2Java {

	private static final Logger LOG = Logger
			.getLogger(OWL2Java.class.getName());

	// CLI map
	private static final Map<COMMAND, OptionParser> map = new HashMap<COMMAND, OptionParser>();

	static {
		map.put(COMMAND.help, new OptionParser() {
			{
				// no options
			}
		});
		map.put(COMMAND.transform, new OptionParser() {
			{

				accepts("m", "mapping file").withRequiredArg().ofType(
						String.class);
				accepts("p", "package").withRequiredArg().ofType(String.class)
						.defaultsTo("generated");
				accepts("c", "context name").withRequiredArg().ofType(
						String.class);
				accepts("d", "output directory").withRequiredArg().ofType(
						String.class).defaultsTo(".");
			}
		});
		map.put(COMMAND.list, new OptionParser() {
			{
				accepts("m", "mapping file").withRequiredArg().ofType(
						String.class);
				accepts("p", "package").withRequiredArg().ofType(String.class)
						.defaultsTo("generated");
			}
		});
	}

	private enum COMMAND {
		help, list, transform;
	}

	private static void printHelp(COMMAND cc) {
		switch (cc) {
		case help:
			System.out
					.println("Help command gives hints on how to use other commands. Try 'OWL2Java help <command>' for more specific info.");
			System.out.println("");
			System.out.println("Syntax: OWL2Java help <command>");
			System.out.println("");
			break;
		case list:
			System.out.println("Lists all available IC contexts.");
			System.out.println("");
			System.out
					.println("Syntax: OWL2Java list <ontology_uri> [ <options> ].");
			System.out.println("");
			break;
		case transform:
			System.out
					.println("Transforms all ICs into annotated Java classes.");
			System.out.println("");
			System.out
					.println("Syntax: OWL2Java transform <ontology_uri> [ <options> ].");
			System.out.println("");
			break;
		}

		try {
			map.get(cc).printHelpOn(System.out);
		} catch (Exception e) {
			LOG.log(Level.SEVERE, e.getMessage(), e);
		}
	}

	public static void main(String[] args) {

		if (args.length == 0) {
			System.out
					.println("Syntax: OWL2Java <command> <args>. Run 'OWL2Java help' for more details");
			return;
		}

		final COMMAND c = COMMAND.valueOf(args[0]);

		final OptionParser op = map.get(c);
		final OptionSet os = op.parse(args);

		final OWL2JavaTransformer oj;

		switch (c) {
		case help:
			if (args.length != 1) {
				final COMMAND cc = COMMAND.valueOf(args[1]);
				printHelp(cc);
			} else {
				System.out.println("Available commands : "
						+ Arrays.asList(COMMAND.values()));
			}

			break;
		case list:
			if (os.has("m")) {
				oj = new OWL2JavaTransformer(os.nonOptionArguments().get(
						os.nonOptionArguments().size() - 1), os.valueOf("m")
						.toString());
			} else {
				oj = new OWL2JavaTransformer(os.nonOptionArguments().get(
						os.nonOptionArguments().size() - 1), null);
			}

			LOG.info("Available contexts: " + oj.listContexts());
			break;
		case transform:
			if (!os.has("c")) {
				LOG
						.severe("The parameter '-c' is obligatory. Try the 'help' command for more details.");
				break;
			}

			if (os.has("m")) {
				oj = new OWL2JavaTransformer(os.nonOptionArguments().get(
						os.nonOptionArguments().size() - 1), os.valueOf("m")
						.toString());
			} else {
				oj = new OWL2JavaTransformer(os.nonOptionArguments().get(
						os.nonOptionArguments().size() - 1), null);
			}

			if (!oj.listContexts().contains(os.valueOf("c"))) {
				LOG.severe("The parameter '-c' is invalid. Found contexts: "
						+ oj.listContexts());
				break;
			}

			oj
					.transform(os.valueOf("c").toString(), os.valueOf("p")
							.toString(), os.valueOf("d").toString());

			break;
		default:
			System.out.println("Unknown command '" + args[0]
					+ "', try 'OWL2Java help.'");
		}
	}
}
