package atom_finder;

import org.eclipse.cdt.internal.core.dom.rewrite.astwriter.*;

// The real ASTWriterVisitory adds superfluous parens to cast expressions
// This writer does not

public class SanitaryASTWriterVisitor extends ASTWriterVisitor {
	public SanitaryASTWriterVisitor() {
		super();
		expWriter = new SanitaryExpressionWriter(scribe, this, macroHandler, commentMap);
	}
}
