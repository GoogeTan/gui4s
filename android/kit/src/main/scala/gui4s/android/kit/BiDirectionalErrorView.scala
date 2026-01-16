package gui4s.android.kit

import android.content.Context
import android.graphics.Color
import android.graphics.Typeface
import android.view.ViewGroup
import android.widget.FrameLayout
import android.widget.HorizontalScrollView
import android.widget.ScrollView
import android.widget.TextView


def createBiDirectionalErrorView(context: Context, errorText: String): ScrollView = {
  val verticalScrollView = new ScrollView(context)
  verticalScrollView.setLayoutParams(new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT))
  verticalScrollView.setFillViewport(true)
  verticalScrollView.setBackgroundColor(Color.parseColor("#FFF0F0")) // Light red background

  val horizontalScrollView = new HorizontalScrollView(context)
  horizontalScrollView.setLayoutParams(new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT))
  val textView = new TextView(context)
  textView.setLayoutParams(new FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT))
  textView.setText(errorText)
  textView.setTextColor(Color.parseColor("#B00020")) // Dark red text

  textView.setTypeface(Typeface.MONOSPACE) // Monospace for stack trace alignment

  textView.setTextSize(14f)
  val padding = (16 * context.getResources.getDisplayMetrics.density).toInt
  textView.setPadding(padding, padding, padding, padding)
  horizontalScrollView.addView(textView)
  verticalScrollView.addView(horizontalScrollView)
  verticalScrollView
}