package atom_finder;

import org.eclipse.cdt.internal.core.dom.rewrite.astwriter.*;
import org.eclipse.cdt.core.dom.ast.IASTCastExpression;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTCastExpression;
import org.eclipse.cdt.internal.core.dom.rewrite.commenthandler.NodeCommentMap;

// The real ExpressionWriter adds superfluous parens to cast expressions
// This writer does not

public class SanitaryExpressionWriter extends ExpressionWriter{
	private static final String STATIC_CAST_OP = "static_cast<"; //$NON-NLS-1$
	private static final String REINTERPRET_CAST_OP = "reinterpret_cast<"; //$NON-NLS-1$
	private static final String DYNAMIC_CAST_OP = "dynamic_cast<"; //$NON-NLS-1$
	private static final String CONST_CAST_OP = "const_cast<"; //$NON-NLS-1$
	private static final String CLOSING_CAST_BRACKET_OP = ">"; //$NON-NLS-1$
	private static final String CLOSING_BRACKET_OP = ")"; //$NON-NLS-1$
  private static final String OPEN_BRACKET_OP = "("; //$NON-NLS-1$

	public SanitaryExpressionWriter(Scribe scribe, ASTWriterVisitor visitor, MacroExpansionHandler macroHandler, NodeCommentMap commentMap) {
		super(scribe, visitor, macroHandler, commentMap);
	}

	protected void writeExpression(IASTExpression expression) {
		if (expression instanceof IASTCastExpression) {
			writeCastExpression((IASTCastExpression) expression);
		} else {
			super.writeExpression(expression);
    }
  }

	private void writeCastExpression(IASTCastExpression castExp) {
		scribe.print(getCastPrefix(castExp.getOperator()));
		castExp.getTypeId().accept(visitor);
		scribe.print(getCastPostfix(castExp.getOperator()));
		castExp.getOperand().accept(visitor);
	}

  // Verbatim copy-paste
	private String getCastPrefix(int castType) {
		switch (castType) {
		case IASTCastExpression.op_cast:
			return OPEN_BRACKET_OP;
		case ICPPASTCastExpression.op_const_cast:
			return CONST_CAST_OP;
		case ICPPASTCastExpression.op_dynamic_cast:
			return DYNAMIC_CAST_OP;
		case ICPPASTCastExpression.op_reinterpret_cast:
			return REINTERPRET_CAST_OP;
		case ICPPASTCastExpression.op_static_cast:
			return STATIC_CAST_OP;
		default:
			throw new IllegalArgumentException("Unknown Cast Type"); //$NON-NLS-1$
		}
	}

	private String getCastPostfix(int castType) {
		switch (castType) {
		case IASTCastExpression.op_cast:
			return CLOSING_BRACKET_OP;
		case ICPPASTCastExpression.op_const_cast:
		case ICPPASTCastExpression.op_dynamic_cast:
		case ICPPASTCastExpression.op_reinterpret_cast:
		case ICPPASTCastExpression.op_static_cast:
			return CLOSING_CAST_BRACKET_OP;
		default:
			throw new IllegalArgumentException("Unknown Cast Type"); //$NON-NLS-1$
		}
	}
}

