from manim import *
import math


class Intro(Scene):
	def construct(self):

		# self.add(NumberPlane().set_o5pacity(0.3))

		shm_text=Text("সরল ছন্দিত স্পন্দন", font="Siyam Rupali").scale(0.7).to_edge(UP)
		theta = ValueTracker(0)

		#Pendulum
		pendulum_center = [-4, 2, 0]
		dot = Dot().set_color(RED) #Pendulum Bob
		pendulum_radius = 2
		arc = Arc(arc_center = pendulum_center, 
			radius = pendulum_radius, 
			start_angle = 1.5*PI-.5, 
			angle=1).set_stroke(GRAY) 
		
		#Oscillating Dot
		oscillating_dot = Dot().set_color(BLUE_C)
		oscillating_dot_line = DashedLine([0, 0, 0], [0, 2, 0], dashed_ratio = 0.3)
		
		#Sine Wave
		x_axis = Line([-1.5*PI, -2, 0], [1.5*PI, -2, 0])
		y_axis = Line([x_axis.get_start()[0],-3,0], [x_axis.get_start()[0],-1, 0])
		graph_dot = Dot([x_axis.get_start()[0],-2,0]).set_color(YELLOW)
		sine_wave = VMobject()
		sine_wave.set_points_as_corners([graph_dot.get_center(), graph_dot.get_center()])
		def update_path(sine_wave):
			if (.5*theta.get_value())<=(3*PI):
				previous_sine_wave = sine_wave.copy()
				previous_sine_wave.add_points_as_corners([graph_dot.get_center()])
				sine_wave.become(previous_sine_wave)
		sine_wave.add_updater(update_path)

		#Spring
		spring_start_point = [2, 1, 0]
		spring_end_point = [5, 1, 0]
		spring = DashedLine(spring_start_point, spring_end_point, dashed_ratio = 0.4)
		Object = Dot(color = BLUE_C).scale(2)

		#Updaters
		graph_dot.add_updater(
			lambda x: x.move_to([
				min(x_axis.get_start()[0]+(.5*theta.get_value())%(3*PI), x_axis.get_start()[0]+3*PI),  
				-2+math.sin(theta.get_value()) if graph_dot.get_center()[0]<(-4+3*PI) else -2, 
				0])
			)
		pendulum_string = Line().add_updater(
			lambda x: x.become(
				Line(pendulum_center, dot.get_center())
				)
			)
		dot.add_updater(
			lambda x: x.move_to(
				[pendulum_center[0]+pendulum_radius*math.sin(.5*math.sin(theta.get_value())), 
				pendulum_center[1]-pendulum_radius*math.cos(.5*math.sin(theta.get_value())), 
				0]
				)
			)
		oscillating_dot.add_updater(
			lambda x: x.move_to(
				Dot([0, 1+math.sin(theta.get_value()),0])
				)
			)
		spring.add_updater(
			lambda x: x.stretch_to_fit_width(2.5+math.sin(theta.get_value())).move_to(spring_start_point, 
				aligned_edge = LEFT))
		Object.add_updater(lambda x: x.next_to(spring, RIGHT, buff = 0))


		self.wait()
		self.play(Write(shm_text))
		self.add(sine_wave)
		self.play(*[Create(i) for i in (spring, Object, dot, pendulum_string, oscillating_dot, 
			oscillating_dot_line, y_axis, x_axis, graph_dot)])
		self.play(theta.animate.set_value(11*PI), run_time = 11, rate_func = linear)
		self.play(theta.animate.set_value(12*PI), FadeOut(*self.mobjects),	rate_func = linear)
		self.wait()



class Fkx(Scene):
	def construct(self):
		# self.add(NumberPlane().set_opacity(0.5))

		theta = ValueTracker(0)

		spring_start_point = [-2, 0, 0]
		spring_end_point = [2,0,0]
		spring_Stretch_Point = [3,0,0]
		spring_Squeeze_Point = [1,0,0]

		Wall = Line([spring_start_point[0], -.2, 0], [spring_start_point[0], .2, 0])
		spring = DashedLine(spring_start_point, spring_end_point, dashed_ratio = 0.4, color = MAROON_A)
		Object = Dot(color = BLUE_C).scale(2).next_to(spring, RIGHT, buff = 0)

		Equiline = Line([2, 0, 0], [2, -1, 0])
		O = Text("সাম্যাবস্থা", font="Siyam Rupali").scale(0.3).next_to(Equiline, DOWN)

		FLeftArrow = Arrow()
		xRightArrow = Arrow()

		F = MathTex(r"\vec{F}").scale(0.8).next_to(FLeftArrow, UP, buff = SMALL_BUFF)
		x = MathTex(r"\vec{x}").scale(0.8).next_to(xRightArrow, DOWN, buff = SMALL_BUFF)
		disclaimer = Text("Vectors are not to scale").scale(0.5).to_corner(DL)
		
		Law = MathTex(r"\vec{F} \propto \vec{x}").to_corner(DR)
		Law2 = MathTex(r"\vec{F} = -k \vec{x}").to_corner(DR)


		Object.add_updater(lambda x: x.next_to(spring, RIGHT, buff = 0))
		spring.add_updater(
			lambda x: x.stretch_to_fit_width(4+math.sin(theta.get_value())).move_to(spring_start_point, 
				aligned_edge = LEFT))
		FLeftArrow.add_updater(
			lambda x: x.become(Arrow(spring.get_end() + UP*MED_SMALL_BUFF,
				spring_end_point+UP*MED_SMALL_BUFF, 
				max_tip_length_to_length_ratio = .15,
				stroke_width = 4, 
				max_stroke_width_to_length_ratio=10000, buff = 0))
			)
		F.add_updater(lambda x: x.next_to(FLeftArrow.get_tip(), UP,  buff= SMALL_BUFF))
		xRightArrow.add_updater(
			lambda x: x.become(Arrow(spring_end_point + DOWN*MED_SMALL_BUFF, 
				spring.get_end()+DOWN*MED_SMALL_BUFF, 
				max_tip_length_to_length_ratio = .15,
				stroke_width = 4,
				max_stroke_width_to_length_ratio=10000,  buff = 0))
			)
		x.add_updater(lambda x: x.next_to(xRightArrow.get_tip(), DOWN,   buff = SMALL_BUFF))

		self.play(*[Create(i) for i in (Object, spring, Wall)])
		self.play(Write(O), Create(Equiline))
		self.wait()
		self.play(theta.animate.set_value(2*PI), rate_func = linear, run_time = 2)
		self.wait()
		self.play(theta.animate.set_value(2.5*PI), rate_func = linear, run_time = 1.75)
		self.play(*[Write(i) for i in (x, xRightArrow, F, FLeftArrow)])
		self.wait()
		self.play(*[FadeOut(i) for i in (x, xRightArrow, F, FLeftArrow)])
		self.play(theta.animate.set_value(3.5*PI), rate_func = linear, run_time = 3.5)
		self.wait(2)
		self.play(*[Write(i) for i in (x, xRightArrow, F, FLeftArrow)])
		self.wait()
		self.play(theta.animate.set_value(8.5*PI), rate_func = linear, run_time = 7.5)
		self.wait()
		self.play(Write(Law))
		self.wait()
		self.play(ReplacementTransform(Law, Law2))
		self.wait()
		self.play(*[FadeOut(i) for i in (Object, spring, Wall, O, Equiline, x, xRightArrow, F, FLeftArrow)])
		self.wait()
		self.play(FadeOut(Law2))
		self.wait(3)

		#CUT to Heavy Math

		a = MathTex(r"\vec{a} = -\frac{k}{m}\vec{x}").move_to([0, -2.5, 0])
		F = MathTex(r"\vec{a}").scale(0.8).next_to(FLeftArrow, UP, buff = SMALL_BUFF)
		theta.set_value(.5*PI)

		self.remove(Law)
		self.wait()
		self.add(a)
		self.wait()
		self.play(*[Create(i) for i in (spring, Object, Equiline, O, x, xRightArrow, F, FLeftArrow)])
		self.wait()
		self.play(theta.animate.set_value(10*PI), rate_func = linear, run_time = 10)
		self.play(*[FadeOut(i) for i in (spring, Object, Equiline, O, x, xRightArrow, F, 
			FLeftArrow)])



class HeavyMath(Scene):
	def construct(self):
		Law = MathTex(r"\vec{F} = -k\vec{x}").to_corner(DR)
		ra = MathTex(r"\Rightarrow").to_corner(UL)
		Line = []

		Line.append(MathTex(r"\Rightarrow m\vec{a} = -k\vec{x}").next_to(ra, DOWN, 
			aligned_edge = LEFT, buff = MED_LARGE_BUFF))
		Line.append(VGroup(MathTex(r"\Rightarrow"), 
			MathTex(r"\vec{a} = -\frac{k}{m}\vec{x}")).arrange(RIGHT).next_to(Line[0], DOWN, 
			aligned_edge = LEFT))
		diffEq = VGroup(
			MathTex(r"\Rightarrow"), MathTex(r"\frac{d^2 \vec{x}}{dt^2}"), MathTex(r"="), 
			MathTex(r"-"), MathTex(r"\frac{k}{m}"), MathTex(r"\vec{x}")
			).arrange(RIGHT).next_to(Line[1], DOWN, aligned_edge = LEFT)
		diffEqPart = VGroup(diffEq[1], diffEq[2], diffEq[3], diffEq[4], diffEq[5])

		self.add(Law)
		self.play(ApplyMethod(Law.next_to, ra, RIGHT), run_time = 2)
		self.play(TransformFromCopy(Law, Line[0]))
		self.wait()
		self.play(TransformFromCopy(Line[0], Line[1]))
		self.wait()
		self.play(ApplyMethod(Line[1][1].move_to, [0,-2.5,0]), FadeOut(Law), 
			FadeOut(Line[0]), FadeOut(Line[1][0]))
		#Cut to Frequency Part 2
		self.wait()
		self.play(ApplyMethod(Line[1][1].next_to, Line[1][0], RIGHT, buff = 0), 
			FadeIn(Law), FadeIn(Line[0]), FadeIn(Line[1][0]))
		self.play(TransformFromCopy(Line[1], diffEq))
		self.wait()
		self.play(ApplyMethod(diffEqPart.move_to, ORIGIN))
		self.wait()
		self.play(FadeOut(Law), 
			FadeOut(Line[0]), 
			FadeOut(Line[1]), 
			FadeOut(diffEq[0]))
		self.play(Indicate(diffEq[1]))
		self.wait()
		self.play(Indicate(diffEq[5]))
		self.wait()
		self.play(Indicate(diffEq[4]))
		self.wait(3)



class DefinitionIntro(Scene):
	def construct(self):
		text = Text("কিন্তু তার আগে একটু সরল ছন্দিত স্পন্দন সম্পর্কিত কিছু বিষয় শিখে নিই", 
			font = "Siyam Rupali").scale(0.5)
		self.wait()
		self.play(Write(text), run_time = 3)
		self.wait()



class Period(Scene):
	def construct(self):
		# self.add(NumberPlane().set_opacity(0.5))

		title = Text("পর্যায়কাল(T)", font = "Siyam Rupali").scale(0.7).to_edge(UP)
		PeriodDefinition = Text("পর্যায়কাল: একটি পূর্ণস্পন্দনের জন্য প্রয়োজনীয় সময়", 
			font = "Siyam Rupali").scale(0.4)
		PeriodDefinition.to_corner(DL)

		theta = ValueTracker(0)

		#Spring
		spring_start_point = 2*LEFT
		spring_end_point = 2*RIGHT
		spring = DashedLine(spring_start_point, spring_end_point, dashed_ratio = 0.4)
		Object = Dot(color = BLUE_C).scale(2)
		Equiline = Line([2, .2, 0], [2, -.2, 0], z_index = Object.z_index-1)

		spring.add_updater(
			lambda x: x.stretch_to_fit_width(4+math.sin(theta.get_value())).move_to(spring_start_point,
				aligned_edge = LEFT)
			)
		Object.add_updater(
			lambda x: x.move_to(spring.get_end()))

		T1Text = MathTex(r"T = 2s").scale(0.7).move_to(DOWN+2*RIGHT)
		T2Text = MathTex(r"T = 4s").scale(0.7).move_to(DOWN+2*RIGHT)
		T3Text = MathTex(r"T = 1s").scale(0.7).move_to(DOWN+2*RIGHT)

		self.play(Write(title))
		self.wait()
		self.play(*[Create(i) for i in (spring, Object, Equiline)])
		self.play(Write(PeriodDefinition))
		self.wait()
		self.play(theta.animate.set_value(1*PI), Write(T1Text), rate_func =linear, run_time = 1)

		self.play(theta.animate.set_value(6*PI), rate_func =linear, run_time = 5)
		theta.set_value(0)
		self.wait()

		self.play(theta.animate.set_value(.25*PI), ReplacementTransform(T1Text, T2Text),
			rate_func =linear, run_time = .5)
		self.play(theta.animate.set_value(4*PI), rate_func =linear, run_time = 7.5)
		theta.set_value(0)
		self.wait()

		self.play(theta.animate.set_value(1*PI), ReplacementTransform(T2Text, T3Text),
			rate_func =linear, run_time = .5)
		self.play(theta.animate.set_value(12*PI), rate_func =linear, run_time = 5.5)
		self.wait()



class Amplitude(Scene):
	def construct(self):
		# self.add(NumberPlane().set_opacity(0.5))

		title=Text("বিস্তার(A)", font="Siyam Rupali").scale(0.7).to_edge(UP)
		AmplitudeDefinition = Text("বিস্তার: সাম্যাবস্থা থেকে একদিকে সর্বোচ্চ দুরত্ব", 
			font = "Siyam Rupali").scale(0.4).to_corner(DL)


		theta = ValueTracker(0)

		#Pendulum
		pendulum_center = [-4, 1, 0]
		pendulum_radius = 2
		dot = Dot().set_color(YELLOW) #Pendulum Bob
		arc = Arc(arc_center = pendulum_center, 
			radius = pendulum_radius, 
			start_angle = 1.5*PI-.5, 
			angle=1, z_index = dot.z_index-1).set_stroke(GRAY)
			
		#oscillating dot
		oscillating_dot_equilibriam = Line([-.2, 0, 0], [.2, 0,0])
		oscillating_dot = Dot().set_color(YELLOW)
		oscillating_dot_line = DashedLine([0, -1, 0], [0, 1, 0], dashed_ratio = 0.3)

		#Spring
		spring_start_point = [2, 0, 0]
		spring_end_point = [5, 0, 0]
		spring_equilibriam = Line([4.5,-.2,0], [4.5,.2,0])
		spring = DashedLine(spring_start_point, spring_end_point, dashed_ratio = 0.4)
		Object = Dot(color = BLUE_C).scale(2)

		Amplitude = BraceBetweenPoints(arc.get_start(), [-4, -1, 0], buff = 0.25)
		Amplitude2 = BraceBetweenPoints([0, -1, 0], [0,0,0], buff = 0.25)
		Amplitude3 = BraceBetweenPoints([4.5,0,0], [5.5,0,0], buff = 0.25)
		
		a = Amplitude.get_tex("A", buff = 0)
		a2= Amplitude2.get_tex("A", buff = 0)
		a3= Amplitude3.get_tex("A", buff = 0)
		
		pendulum_string = Line().add_updater(
			lambda x: x.become(
				Line(pendulum_center, dot.get_center())
				)
			)
		dot.add_updater(
			lambda x: x.move_to(
				[pendulum_center[0]+pendulum_radius*math.sin(.5*math.sin(theta.get_value())), 
				pendulum_center[1]-pendulum_radius*math.cos(.5*math.sin(theta.get_value())), 
				0]
				)
			)
		oscillating_dot.add_updater(
			lambda x: x.move_to(
				Dot([0, math.sin(theta.get_value()),0])
				)
			)

		spring.add_updater(
			lambda x: x.stretch_to_fit_width(2.5+math.sin(theta.get_value())).move_to(spring_start_point, 
				aligned_edge = LEFT))
		Object.add_updater(lambda x: x.move_to(spring.get_end()))


		self.wait()
		# self.add(arc1)
		self.play(Write(title))
		self.wait()
		self.play(*[Create(i) for i in (dot, pendulum_string, oscillating_dot,
			oscillating_dot_line, spring, Object, AmplitudeDefinition)])
		self.wait()
		self.play(theta.animate.set_value(3*PI), run_time = 3, rate_func = linear)
		self.play(theta.animate.set_value(4*PI), Create(arc), Create(spring_equilibriam), 
			Create(oscillating_dot_equilibriam), rate_func = linear)
		self.play(*[Write(i) for i in (Amplitude, Amplitude2, Amplitude3, a, a2, a3)], 
			theta.animate.set_value(5*PI), rate_func = linear)
		self.play(theta.animate.set_value(10*PI), run_time = 5, rate_func = linear)
		self.wait(2)



class FourFrequency(Scene):
	def construct(self):
		title = Text("কম্পাঙ্ক(f)", font = "Siyam Rupali").scale(0.7).to_edge(UP)
		self.add(NumberPlane().set_opacity(0.5))

		theta = ValueTracker(0)
		
		#Spring 1
		spring_start_point = [[-5.5, 1, 0], [1.5, 1, 0], [-5.5, -1.5, 0], [1.5, -1.5, 0]]
		spring_end_point = [[-2.5, 1, 0], [4.5, 1, 0], [-2.5, -1.5, 0], [4.5, -1.5, 0]]
		spring = []
		Object = []
		for i in range(0, 4):
			spring.append(DashedLine(spring_start_point[i], spring_end_point[i], dashed_ratio = 0.4))
			Object.append(Dot(color = BLUE_C).scale(2))


		spring[0].add_updater(
			lambda x: x.stretch_to_fit_width(3+math.sin(.8*theta.get_value())).move_to(spring_start_point[0], 
				aligned_edge = LEFT))
		Object[0].add_updater(lambda x: x.move_to(spring[0].get_end()))
		spring[1].add_updater(
			lambda x: x.stretch_to_fit_width(3+math.sin(.5*theta.get_value())).move_to(spring_start_point[1], 
				aligned_edge = LEFT))
		Object[1].add_updater(lambda x: x.move_to(spring[1].get_end()))
		spring[2].add_updater(
			lambda x: x.stretch_to_fit_width(3+math.sin(1.2*theta.get_value())).move_to(spring_start_point[2], 
				aligned_edge = LEFT))
		Object[2].add_updater(lambda x: x.move_to(spring[2].get_end()))
		spring[3].add_updater(
			lambda x: x.stretch_to_fit_width(3+math.sin(1.5*theta.get_value())).move_to(spring_start_point[3], 
				aligned_edge = LEFT))
		Object[3].add_updater(lambda x: x.move_to(spring[3].get_end()))

		self.play(Write(title))
		self.play(*[Create(i) for i in (*[spring[j] for j in range(0,4)], *[Object[j] for j in range(0,4)])])
		self.play(theta.animate.set_value(6*PI), run_time = 6, rate_func = linear)

		self.wait()



class CircleAndDot(Scene):
	def construct(self):
		# self.add(NumberPlane().set_opacity(0.5))


		r = 3
		offset = 1.5
		theta=ValueTracker(0)

		corner_math = VGroup(MathTex(r"\vec{F}=-k\vec{x}"), 
			MathTex(r"\Rightarrow \vec{a} = -\frac{k}{m}\vec{x}")).arrange(DOWN, aligned_edge = LEFT)
		corner_math.scale(0.7).to_corner(DR)

		ref_circle = Circle(arc_center = ([-offset, 0, 0]), radius = r)
		ref_circle_center = Dot(ref_circle.get_center())
		dot_on_circle = Dot(ref_circle.get_center()+[r,0,0])
		dot_on_circle_label = Text("P").scale(0.5).next_to(dot_on_circle)
		ωDefinition = VGroup(
			Text("কৌণিক বেগ", font = "Siyam Rupali"), 
			Text("ω", font = "Arial"), Text("অর্থাৎ বৃত্তটি একক সময়ে", font = "Siyam Rupali"),
			Text("ω", font = "Arial"), Text("রেডিয়ান কোণ ঘোরে", font = "Siyam Rupali")
			).arrange(RIGHT).scale(0.5).to_corner(DL)

		oscillating_dot_line = Line([r+2.5-offset, -r, 0 ], [r+2.5-offset, r, 0])
		oscillating_dot = Dot([r+2.5-offset, r*math.sin(.3*PI), 0]).set_color(BLUE_C)
		oscillating_dot_label = Text("P'").scale(0.5).next_to(oscillating_dot)
		oscillating_dot_eq = Line([r+2.5-offset-.1, 0, 0], [r+2.5-offset+.1, 0, 0])



		line = DashedLine(dot_on_circle, oscillating_dot).set_opacity(0.5)
		line_x = Line(ref_circle.get_center(), dot_on_circle.get_center())
		line_radius = Line(ref_circle.get_center(), [-offset+r*math.cos(.3*PI),r*math.sin(.3*PI),0])
		a = Angle(line_x, line_radius, radius = 0.5, other_angle = False)
		thetatext = MathTex(r"\theta").scale(0.7).move_to([-.8, .3, 0])
		radius_brace = Brace(line_radius, direction=line_radius.copy().rotate(PI/2).get_unit_vector())
		radius_brace_text = radius_brace.get_tex("r", buff = 0)
		radius_y_projection = Line(line_radius.get_end(), [line_radius.get_end()[0], 0, 0])
		radius_y_projection_brace = Brace(radius_y_projection, 
			direction = radius_y_projection.copy().rotate(PI/2).get_unit_vector())
		radius_y_projection_brace_text = radius_y_projection_brace.get_tex("r \\sin \\theta", buff = 0.2)

		x_brace = BraceBetweenPoints(oscillating_dot_line.get_center(), oscillating_dot.get_center())
		x_brace_text = x_brace.get_tex("x = r \\sin \\theta", buff = 0.2)
		note = MathTex(r"\vec{P'_x} = \vec{r} \sin \theta").to_corner(DR)


		LightSource = oscillating_dot_line.copy().to_edge(LEFT).set_color(YELLOW)
		LightText = Text("Light Source").scale(0.5)
		LightText.next_to(LightSource, UP, aligned_edge = LEFT).shift(UP/3)
		LightRay = []
		for i in range(0, 20):
			LightRay.append(Arrow([LightSource.get_center()[0]+.2, -r+(i*2*r/19), 0], 
				[LightSource.get_center()[0]+.4, -r+(i*2*r/19), 0], 
				max_tip_length_to_length_ratio = .5).set_color(YELLOW))
		LightArrow = Arrow(
			LightText.get_center()+LEFT*.5+DOWN*.25, [-6.5, 3, 0], 
			max_tip_length_to_length_ratio = 0.15, buff = 0)

		oscillating_dot.add_updater(lambda x: x.move_to([r+2.5-offset, r*math.sin(theta.get_value()),0]))
		dot_on_circle.add_updater(
			lambda x: x.move_to([r*math.cos(theta.get_value())-offset, r*math.sin(theta.get_value()), 0])
			)
		line.add_updater(
			lambda x: x.become(DashedLine(dot_on_circle, oscillating_dot).set_opacity(0.5))
			)
		line_radius.add_updater(
			lambda x: x.put_start_and_end_on(line_x.get_start(), dot_on_circle.get_center())
			)
		dot_on_circle_label.add_updater(
			lambda x: x.next_to(dot_on_circle))

		self.wait()
		self.play(Create(ref_circle_center), Create(ref_circle))
		self.play(Create(dot_on_circle), Write(dot_on_circle_label))
		self.wait()
		self.play(FadeOut(dot_on_circle_label), run_time=0.5)
		self.play(theta.animate.set_value(.5*PI), Write(ωDefinition), run_time = 1, rate_func = linear)
		self.play(theta.animate.set_value(1.5*PI), run_time = 2, rate_func = linear)
		self.play(theta.animate.set_value(2*PI), FadeOut(ωDefinition), run_time = 1, rate_func = linear)
		self.play(theta.animate.set_value(2.7*PI), run_time = 1.4, rate_func = linear)
		self.play(FadeIn(dot_on_circle_label), run_time= 0.5)

		self.play(Create(LightSource), Create(LightText), Create(LightArrow), 
			*[Create(LightRay[i]) for i in range(0, len(LightRay))])
		self.wait()
		self.play(FadeOut(LightText), FadeOut(LightArrow))

		self.play(Create(oscillating_dot_line), Create(oscillating_dot_eq))
		self.play(Create(oscillating_dot), Write(oscillating_dot_label), Write(line)) 
		self.wait()
		self.play(FadeOut(oscillating_dot_label), FadeOut(dot_on_circle_label), run_time = 0.5)
		self.play(theta.animate.set_value(8.3*PI), run_time = 11.2, rate_func = linear)
		self.wait()
		self.play(Write(corner_math))
		self.wait()
		self.play(FadeOut(corner_math))


		# CUT

		# self.play(Create(line_x), Create(a), Write(thetatext), Create(radius_brace), 
		# 	Write(radius_brace_text), Write(dot_on_circle_label), 
		# 	Write(oscillating_dot_label), Create(line_radius))

		# self.wait()

		# self.play(Create(radius_y_projection), Create(radius_y_projection_brace), 
		# 	Write(radius_y_projection_brace_text))
		# self.play(Create(x_brace), Write(x_brace_text))

		# self.wait()

		# self.play(TransformFromCopy(x_brace_text, note))


		# self.play(FadeOut(a), FadeOut(thetatext), FadeOut(VGroup(x_brace, radius_brace, 
		# 	radius_y_projection_brace, radius_y_projection, radius_y_projection_brace_text, 
		# 	radius_brace_text, line_radius, x_brace_text)))

		# self.play(theta.animate.set_value(12*PI), run_time = 3.4, rate_func = linear)
		self.wait(5)



class CircleAndDotMath(Scene):
	def construct(self):
		# self.camera.background_color = "#17171c"

		r = 3
		offset = 1.5
		ref_circle = Circle(arc_center = ([-offset, 0, 0]), radius = r)
		ref_circle_center = Dot(ref_circle.get_center())
		dot_on_circle = Dot(ref_circle.get_center()+[r*math.cos(.3*PI), r*math.sin(.3*PI), 0])
		dot_on_circle_label = Text("P").scale(0.5).next_to(dot_on_circle)
		ref_circle_center_label = Text("O").scale(0.7).next_to(ref_circle_center, DOWN)


		oscillating_dot_line = Line([r+2.5-offset, -r, 0 ], [r+2.5-offset, r, 0])
		oscillating_dot_eq = Line([r+2.5-offset-.1, 0, 0], [r+2.5-offset+.1, 0, 0])
		oscillating_dot = Dot([r+2.5-offset, r*math.sin(.3*PI), 0], 
			z_index = oscillating_dot_line.z_index +1).set_color(BLUE_C)
		oscillating_dot_label = Text("P'").scale(0.5).next_to(oscillating_dot)

		line = DashedLine(dot_on_circle, oscillating_dot).set_opacity(0.5) #Line between circle and oscillation
		line_x = Line(ref_circle.get_center(), ref_circle.get_center()+[r,0,0])
		line_x_rotated = line_x.copy().rotate(angle = .3, about_point = ref_circle_center.get_center())
		line_radius = Line(ref_circle.get_center(), [-offset+r*math.cos(.3*PI),r*math.sin(.3*PI),0])
		radius_vector = Arrow(ref_circle.get_center(), [-offset+r*math.cos(.3*PI),r*math.sin(.3*PI),0],
		max_tip_length_to_length_ratio= 0.08,  buff = 0).set_color(BLUE_C)

		radius_brace = Brace(radius_vector, direction=radius_vector.copy().rotate(PI/2).get_unit_vector())
		radius_brace_text = radius_brace.get_tex("\\vec{r}", buff = 0)
		radius_y_projection = Arrow([radius_vector.get_end()[0], 0, 0], radius_vector.get_end(), 
			max_tip_length_to_length_ratio = 0.08, buff = 0).set_color(BLUE_C)
		radius_y_projection_line = Line([radius_vector.get_end()[0], 0, 0], radius_vector.get_end())
		radius_y_projection_brace = Brace(radius_y_projection, 
			direction = radius_y_projection.copy().rotate(3*PI/2).get_unit_vector())
		radius_y_projection_brace_text = radius_y_projection_brace.get_tex("\\vec{r} \\sin \\theta", buff = 0.2)
		
		x = Arrow(oscillating_dot_line.get_center(), oscillating_dot.get_center(), 
			max_tip_length_to_length_ratio = 0.08, buff = 0).set_color(BLUE_C)
		x_brace = Brace(x, direction = x.copy().rotate(3*PI/2).get_unit_vector())
		x_brace_text = x_brace.get_tex("\\vec{x} = \\vec{r} \\sin \\theta", buff = 0.2)
		note = VGroup(MathTex(r"\vec{P'_x}"), MathTex(r"="), MathTex(r"\vec{r}"), 
			MathTex(r"\sin"), MathTex(r"\theta")
			).arrange(RIGHT, buff = .08).to_corner(DR, buff = 0.25).shift(LEFT)
		
		noteB = MathTex(r"(\omega t)").next_to(note[3], RIGHT, buff = 0.1)
		noteC = MathTex(r"(\omega t + \delta)").next_to(note[3], RIGHT, buff = 0.1)


		a = Angle(line_x, radius_vector, radius = 0.5, other_angle = False)
		a2 = Angle(line_x, line_x_rotated, radius = 0.6, other_angle = False)


		thetatext = MathTex(r"\theta").scale(0.7).move_to([-.8, .3, 0])
		theta_omega_text = MathTex(r"\theta = \omega t").scale(0.7).move_to(thetatext, aligned_edge = LEFT)
		delta = MathTex(r"\delta").scale(0.5).move_to([-.5, 0.13, 0])
		theta_omega_text2 = MathTex(r"\omega t").scale(0.7).move_to(thetatext, aligned_edge = LEFT)
		theta_omega_text2.shift(.2*UP)


		acc = Arrow([-offset+r*math.cos(.3*PI),r*math.sin(.3*PI),0], 
			ref_circle.get_center()+[math.cos(.3*PI), math.sin(.3*PI), 0], 
		max_tip_length_to_length_ratio= 0.08,  buff = 0).set_color(BLUE_C)
		acc_brace = Brace(acc, direction = acc.copy().rotate(3*PI/2).get_unit_vector())
		acc_brace_text = acc_brace.get_tex("\\vec{a}", buff = 0.2)


		acc_y_projection = Arrow(acc.get_start(), [acc.get_start()[0], acc.get_end()[1], 0], 
			max_tip_length_to_length_ratio = 0.08, buff = 0).set_color(BLUE_C)
		acc_y_projection_brace = Brace(acc_y_projection, 
			direction = acc_y_projection.copy().rotate(PI/2).get_unit_vector())
		acc_y_projection_brace_text = acc_y_projection_brace.get_tex("\\vec{a} \\sin \\theta", 	buff = 0.2)


		ax = Arrow(oscillating_dot.get_center(), [oscillating_dot_line.get_center()[0], acc.get_end()[1], 0], 
			max_tip_length_to_length_ratio = 0.08, buff = 0).set_color(BLUE_C)
		ax_brace = Brace(ax, direction = ax.copy().rotate(PI/2).get_unit_vector())
		ax_brace_text = ax_brace.get_tex("\\vec{a} \\sin \\theta")
		note2 = MathTex(r"\vec{P'_a}=\vec{a} \sin \theta").next_to(note, LEFT, buff = LARGE_BUFF)
		note3 = MathTex(r"\vec{P'_a} = - w^2 \vec{r} \sin \theta").next_to(note, LEFT, buff = LARGE_BUFF)
		note4 = MathTex(r"\vec{P'_a} = - w^2 \vec{P'_x}").next_to(note, LEFT, buff = LARGE_BUFF)



		LightSource = oscillating_dot_line.copy().to_edge(LEFT).set_color(YELLOW)
		LightRay = []
		for i in range(0, 20):
			LightRay.append(Arrow([LightSource.get_center()[0]+.2, -r+(i*2*r/19), 0], 
				[LightSource.get_center()[0]+.4, -r+(i*2*r/19), 0], 
				max_tip_length_to_length_ratio = .5).set_color(YELLOW))		

		self.add(ref_circle, ref_circle_center, dot_on_circle, oscillating_dot, oscillating_dot_line, 
			LightSource, *[LightRay[i] for i in range(0, len(LightRay))], line, oscillating_dot_eq)
		self.wait(2)
		self.play(*[Create(i) for i in (line_x, line_radius, radius_vector, thetatext, ref_circle_center_label, 
			a, dot_on_circle_label, oscillating_dot_label, radius_brace, radius_brace_text)],)
		self.wait()
		self.play(*[Create(i) for i in (radius_y_projection, radius_y_projection_brace, 
			radius_y_projection_brace_text)], ApplyMethod(dot_on_circle_label.shift, UP/3))
		self.wait()
		self.play(*[Create(i) for i in (x, x_brace, x_brace_text)], 
			ApplyMethod(oscillating_dot_label.shift, UP/3))
		self.wait()
		self.play(TransformFromCopy(x_brace_text, note))
		self.play(*[FadeOut(i) for i in (a, thetatext, radius_vector, radius_brace, radius_brace_text, 
			radius_y_projection, radius_y_projection_brace, radius_y_projection_brace_text, x, x_brace, 
			x_brace_text)])
		self.play(*[Create(i) for i in (line_x, line_radius, a, thetatext, ref_circle_center_label, 
			dot_on_circle_label,oscillating_dot_label, acc, acc_brace, acc_brace_text)])
		self.wait()
		self.play(*[Create(i) for i in (radius_y_projection_line, acc_y_projection, 
			acc_y_projection_brace, acc_y_projection_brace_text)])
		self.wait()
		self.play(*[Create(i) for i in (ax, ax_brace, ax_brace_text)])
		self.wait()
		self.play(TransformFromCopy(ax_brace_text, note2))
		self.wait()
		self.play(ReplacementTransform(note2, note3))
		self.wait()
		self.play(ReplacementTransform(note3, note4))
		self.wait()
		self.play(*[FadeOut(i) for i in (ref_circle, ref_circle_center, dot_on_circle, oscillating_dot, 
			oscillating_dot_line, LightSource, *[LightRay[i] for i in range(0, len(LightRay))], 
			line, oscillating_dot_eq, line_x, line_radius, a, thetatext, ref_circle_center_label, 
			dot_on_circle_label,oscillating_dot_label, acc, acc_brace, acc_brace_text, 
			radius_y_projection_line, acc_y_projection, acc_y_projection_brace_text, 
			acc_y_projection_brace, ax, ax_brace, ax_brace_text)])



		# CUT to Comparison

		self.wait()
		self.play(*[FadeIn(i) for i in (ref_circle, ref_circle_center, dot_on_circle, oscillating_dot, 
			oscillating_dot_line, LightSource, *[LightRay[i] for i in range(0, len(LightRay))], 
			line, oscillating_dot_eq, line_x, line_radius, a, thetatext, ref_circle_center_label, 
			dot_on_circle_label,oscillating_dot_label, acc, acc_brace, acc_brace_text, 
			radius_y_projection_line, acc_y_projection, acc_y_projection_brace_text, 
			acc_y_projection_brace, ax, ax_brace, ax_brace_text)]) 
		self.wait()
		# self.play(*[FadeOut(i) for i in (ref_circle, ref_circle_center, dot_on_circle, oscillating_dot, 
		# 	oscillating_dot_line, LightSource, *[LightRay[i] for i in range(0, len(LightRay))], 
		# 	line, oscillating_dot_eq, line_x, line_radius, a, thetatext, ref_circle_center_label, 
		# 	dot_on_circle_label,oscillating_dot_label)]) 

		self.play(*[FadeOut(i) for i in (note4, acc_y_projection, acc, acc_brace, acc_brace_text, 
			radius_y_projection_line, acc_y_projection_brace_text, acc_y_projection_brace, ax, ax_brace, 
			ax_brace_text)])
		self.wait()
		self.play(ReplacementTransform(note[4], noteB), ReplacementTransform(thetatext, theta_omega_text))
		thetatext = MathTex(r"\theta").scale(0.7).move_to([-.8, .3, 0])
		note[4] = MathTex(r"\theta").next_to(note[3], RIGHT, buff = 0.08)
		self.wait()
		self.play(ReplacementTransform(noteB, note[4]), ReplacementTransform(theta_omega_text, thetatext))
		self.play(TransformFromCopy(line_x, line_x_rotated), FadeOut(line_x),
			a.animate.become(Angle(line_x_rotated, radius_vector, radius = 0.5, other_angle = False)), 
			thetatext.animate.shift(0.2*UP))
		self.play(FadeIn(line_x), Create(a2), Write(delta), ReplacementTransform(note[4], noteC),
			ReplacementTransform(thetatext,theta_omega_text2))
		self.play(ApplyMethod(VGroup(note, noteC).shift, 0.4*LEFT))
		self.wait(3)



class Comparison(Scene):
	def construct(self):
		note1 = VGroup(MathTex(r"\vec{P'_x}"), MathTex(r"="), MathTex(r"\vec{r}"), 
			MathTex(r"\sin"), MathTex(r"\theta")
			).arrange(RIGHT, buff = .08).to_corner(DR, buff = 0.25).shift(LEFT)
		note2 = MathTex(r"\vec{P'_a} = - w^2 \vec{P'_x}").next_to(note1, LEFT, buff = LARGE_BUFF)
		ref = note2.copy()
		Law = MathTex(r"\vec{a} = -\frac{k}{m}\vec{x}").move_to(ORIGIN)
		VGroup(Law, note2.copy()).arrange(DOWN, buff = 1)


		self.add(note1, note2)
		self.play(FadeOut(note1), ApplyMethod(note2.move_to, ORIGIN))
		self.play(ApplyMethod(note2.next_to, Law, DOWN, buff = LARGE_BUFF))
		self.play(Write(Law))
		self.wait()
		self.play(ApplyMethod(note2.move_to, ref), FadeOut(Law), FadeIn(note1))
		# self.wait()
		# self.play(ApplyMethod(note1.to_corner, UL), FadeOut(note2))

		self.wait(3)

